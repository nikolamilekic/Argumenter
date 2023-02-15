namespace Argumenter

open System
open System.Collections.Generic
open System.ComponentModel
open System.ComponentModel.DataAnnotations
open System.Reflection
open System.Text
open FSharpPlus
open FSharpPlus.Lens
open FParsec

open ArgumentInfo
open ParserState

type ArgumentParser<'a>() =
    let callingAssembly = Assembly.GetCallingAssembly()
    let relevantTypes =
        callingAssembly.GetTypes()
        |> Seq.filter(fun t -> t.IsAssignableTo typeof<'a>)
        |> Seq.toList

    let Ok = Result.Ok
    let Error = Result.Error

    let singleWordString =
        notFollowedByString "--"
        >>. notFollowedByString "\""
        >>. many1CharsTill anyChar (spaces1 <|> eof)
    let multiWordString =
        skipString "\"" >>. many1CharsTill anyChar (skipString "\"")
    let stringArgument = (multiWordString <|> singleWordString) <?> "string"

    let contentParsers = Dictionary<Type, Parser<_, _>>(
        [
            typeof<string>, stringArgument |>> box
            typeof<double>, pfloat |>> box
            typeof<int8>, pint8 |>> box
            typeof<int16>, pint16 |>> box
            typeof<int32>, pint32 |>> box
            typeof<int64>, pint64 |>> box
            typeof<uint8>, puint8 |>> box
            typeof<uint16>, puint16 |>> box
            typeof<uint32>, puint32 |>> box
            typeof<uint64>, puint64 |>> box
        ] |> Seq.map KeyValuePair.Create)
    let getContentParser (t : Type) =
        match contentParsers.TryGetValue t with
        | true, x -> x
        | _ -> failwith $"{t.Name} is not a supported argument type."

    let parser : Parser<unit, ParserState> =
        let eof = eof <?> ""
        let printHelp = updateUserState (_help .-> true) >>. fail ""
        let rec pendingArgumentsParser () = getUserState >>= fun state ->
            let argumentParsers =
                state ^. _pendingArguments
                |> Seq.map (fun kvp ->
                    let name = kvp ^. _key
                    let required = state ^. _isRequired (kvp ^. _value)
                    let isMainArgument = kvp ^. _argument_isMainArgument
                    let contentParser = getContentParser (kvp ^. _argument_type)
                    let fullArgumentParser =
                        skipStringCI $"--{name}" >>. spaces1 >>. contentParser
                    let finalParser =
                        if isMainArgument then contentParser <|> fullArgumentParser
                        else fullArgumentParser

                    spaces >>. finalParser .>> spaces
                    >>= (fun value ->
                        updateUserState (_assign kvp .-> value))
                    <?> if required then $"--{name.ToLower()}" else $"[--{name.ToLower()}]")
            let commandParsers =
                state ^. _pendingCommands
                |> Seq.map (fun command ->
                    spaces
                    >>. (pstringCI command.Command <|> pstringCI $"--{command.Command}")
                    >>. spaces
                    >>= (fun () -> updateUserState (_currentCommand .-> command))
                    <?> $"{command.Command.ToLower()}")
            argumentParsers
            |> Seq.append commandParsers
            |> Seq.append [|skipStringCI "--help" <?> "--help" >>. printHelp|]
            |> choice
            >>= fun () -> eof <|> pendingArgumentsParser ()

        eof >>. printHelp
        <|> pendingArgumentsParser () >>. getUserState >>= fun state ->
            match state ^. _missingArguments |> Seq.toList with
            | [] -> preturn ()
            | missing ->
                let missingString = String.concat ", " missing
                fail $"The following required arguments are missing: {missingString}"

    let getProperties (t : Type) =
        t.GetProperties(
            BindingFlags.Public
            ||| BindingFlags.DeclaredOnly
            ||| BindingFlags.Instance
            ||| BindingFlags.GetProperty
            ||| BindingFlags.SetProperty
        )
    let getArgumentInfo (info : PropertyInfo) =
        let t = info.PropertyType

        let isNullable = NullabilityInfoContext().Create(info).WriteState = NullabilityState.Nullable
        let isNullableValue = t.IsGenericType && t.GetGenericTypeDefinition() = typeof<Nullable<_>>.GetGenericTypeDefinition()
        let isOption = t.IsGenericType && t.GetGenericTypeDefinition() = typeof<Option<_>>.GetGenericTypeDefinition()
        let isGenericList = t.IsGenericType && t.GetGenericTypeDefinition() = typeof<List<_>>.GetGenericTypeDefinition()
        let requiredAttributeSet = isNull (info.GetCustomAttribute(typeof<RequiredAttribute>)) = false
        let mainArgumentAttributeSet = isNull (info.GetCustomAttribute(typeof<MainArgumentAttribute>)) = false

        let result =
            zero
            |> _isMainArgument .-> mainArgumentAttributeSet
            |> _type .-> t
            |> _assigner .-> info.SetValue
            |> _requirement .->
                if requiredAttributeSet then AlwaysRequired else
                match info.GetCustomAttribute(typeof<RequiredIfAttribute>) with
                | :? RequiredIfAttribute as x -> RequiredIf (x.ArgumentName, x.Value)
                | _ ->
                    if isNullable || isNullableValue || isOption || isGenericList
                    then Optional
                    else AlwaysRequired
            |> _description .->
                match info.GetCustomAttribute(typeof<DescriptionAttribute>) with
                | :? DescriptionAttribute as x -> x.Description
                | _ -> ""

        if isOption || isNullableValue then
            let firstGenericArgument = t.GetGenericArguments().[0]
            result
            |> _type .-> firstGenericArgument
            |> _assigner .-> (fun (o, v) ->
                let value = t.GetConstructor([|firstGenericArgument|]).Invoke([|v|])
                info.SetValue(o, value)
            )
        elif isNullable then result
        elif isGenericList then
            if isNullable || isOption then failwith "Generic lists cannot be nullable or options." else

            result
            |> _type .-> t.GetGenericArguments().[0]
            |> _assigner .-> (fun (o, v) ->
                let list = info.GetValue(o) :?> System.Collections.IList
                list.Add(v) |> ignore
            )
            |> _allowMultipleDefinitions .-> true
        elif t.IsGenericType then
            failwith "Currently, the only supported generic arguments are F# options and generic lists (List<T>, ResizeArray-s in F#), which are used to capture arguments which can be specified more than once."
        else result
        |> fun result ->
            getContentParser (result ^. _type) |> ignore // Check if the type is supported. Will throw if not.
            result
    let rec getCommandInfos results = function
        | [] -> results
        | (currentType, parent)::next ->
            let argumentInfos =
                getProperties currentType
                |> Array.map (fun p -> p.Name, getArgumentInfo p)
                |> Map.ofSeq
            let currentCommandInfo = {
                Command =
                    if parent |> Option.isNone then ""
                    elif currentType.Name.EndsWith("arguments", StringComparison.InvariantCultureIgnoreCase)
                    then currentType.Name.Substring(0, currentType.Name.Length - 9)
                    else currentType.Name
                SupportedArguments = argumentInfos
                Parent = parent
                Description =
                    match currentType.GetCustomAttribute(typeof<DescriptionAttribute>) with
                    | :? DescriptionAttribute as x -> x.Description
                    | _ -> ""
            }
            let children =
                relevantTypes
                |> Seq.filter (fun t ->
                    t.BaseType = currentType &&
                    isNull (t.GetConstructor([||])) = false)
                |> Seq.map (fun t -> t, Some currentCommandInfo)
                |> Seq.toList
            getCommandInfos ((currentCommandInfo, currentType)::results) (children @ next)
    let commandTypes = getCommandInfos [] [(typeof<'a>, None)]
    let rootCommand = commandTypes |> List.find (fun (_, t) -> t = typeof<'a>) |> fst
    let allCommands = commandTypes |> List.map fst
    let mutable initialState =
        zero
        |> _executableName .-> (callingAssembly.GetName().Name + ".exe")
        |> _currentCommand .-> rootCommand
        |> _allCommands .-> allCommands

    member _.Help(state) =
        let sb = StringBuilder()

        let commandPath = state ^. _commandPath
        let commandPathString = String.Join(
            " ",
            commandPath |> Seq.map (fun c -> c.Command.ToLower()) |> Seq.filter (fun s -> s <> ""))
        let currentCommand = state ^. _currentCommand
        sb.Append($"{(state ^. _executableName).ToLower()}") |> ignore
        if commandPathString <> "" then
            sb.Append($" {commandPathString}") |> ignore

        let mainArgument =
            currentCommand.SupportedArguments
            |> Seq.tryFind (view _argument_isMainArgument)
        match mainArgument with
        | Some kvp ->
            let name = kvp ^. _key
            let info = kvp ^. _value
            if state ^. _isRequired info
            then sb.AppendLine($" <{name.ToLower()}>\n") |> ignore
            else sb.AppendLine($" [<{name.ToLower()}>]\n") |> ignore
        | None -> sb.AppendLine("\n") |> ignore

        if currentCommand.Description <> "" then
            sb.AppendLine(currentCommand.Description).AppendLine("") |> ignore

        let subCommands = state ^. _pendingCommands
        let arguments = state ^. _allSupportedArguments |> Seq.filter (view _argument_isMainArgument >> not)
        let maxLength =
            subCommands |> Seq.map (fun c -> c.Command.Length)
            |> Seq.append (arguments |> Seq.map (fun kvp -> kvp.Key.Length))
            |> Seq.max

        if subCommands |> Seq.isEmpty = false then
            sb.AppendLine("SUBCOMMANDS:\n") |> ignore
            for subCommand in subCommands do
                sb.Append($"  {subCommand.Command.ToLower().PadRight(maxLength + 2)}") |> ignore
                if subCommand.Description <> ""
                then sb.AppendLine($"    {subCommand.Description}") |> ignore
                else sb.AppendLine("") |> ignore
            sb.AppendLine("") |> ignore

        let overridesHelp =
            arguments
            |> Seq.exists (fun kvp -> String.Equals(kvp ^. _key, "help", StringComparison.InvariantCultureIgnoreCase))
        let requiredArguments =
            arguments
            |> Seq.filter (fun kvp -> state ^. _isRequired (kvp ^. _value))

        let printArgument kvp =
            let name : string = kvp ^. _key
            sb.Append($"  --{name.ToLower().PadRight(maxLength)}") |> ignore
            let description = kvp ^. _argument_description
            if description <> ""
            then sb.AppendLine($"    {description}") |> ignore
            else sb.AppendLine("") |> ignore

        if requiredArguments |> Seq.isEmpty = false then
            sb.AppendLine("REQUIRED ARGUMENTS:\n") |> ignore
            for kvp in requiredArguments do printArgument kvp
            sb.AppendLine("") |> ignore

        let optionalArguments =
            arguments
            |> Seq.filter (fun kvp -> state ^. _isRequired (kvp ^. _value) |> not)
        sb.AppendLine("OPTIONAL ARGUMENTS:\n") |> ignore
        for kvp in optionalArguments do printArgument kvp
        if not overridesHelp then
            let helpArgument = "help".PadRight(maxLength)
            sb.AppendLine($"  --{helpArgument}    Prints a list of arguments and commands") |> ignore

        sb.ToString()

    member this.Help() = this.Help(initialState)
    member _.WithExecutableName(name : string) =
        initialState <- initialState |> _executableName .-> name
    member _.WithCustomParser<'arg>(parser : Parser<'arg, _>) =
        contentParsers[typeof<'arg>] <- (parser |>> box)
    member this.Parse(args : string) : Result<_, _> = monad.strict {
        let! result =
            match runParserOnString parser initialState "" args with
            | ParserResult.Success (_, state, _) -> Ok state
            | ParserResult.Failure (_, _, state) when state ^. _help ->
                Error (this.Help(state))
            | ParserResult.Failure (message, _, _) when message.StartsWith "Error in" ->
                let withoutFirstLine =
                    message.Split(Environment.NewLine)
                    |> Seq.skip 1
                    |> String.concat(Environment.NewLine)
                Error withoutFirstLine
            | ParserResult.Failure (message, _, _) -> Error message
        let resultCommand = result ^. _currentCommand
        let argumentType =
            commandTypes
            |> List.find (fun (c, _) -> LanguagePrimitives.PhysicalEquality c resultCommand)
            |> snd
        let argumentObject = Activator.CreateInstance argumentType
        for assigner in result ^. _assigners do assigner argumentObject
        return argumentObject :?> 'a
    }
    member this.Parse() : Result<_, _> =
        let firstCommandLineArg = Environment.GetCommandLineArgs()[0]
        let rawArguments = Environment.CommandLine[firstCommandLineArg.Length..].Trim()
        this.Parse(rawArguments)
