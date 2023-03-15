module Argumenter.ArgumentParser

open System
open System.Collections.Generic
open System.ComponentModel
open System.ComponentModel.DataAnnotations
open System.Reflection
open System.Text
open System.Text.Json
open System.Text.Json.Nodes
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Lens
open FParsec

open ArgumentInfo
open ParserState
open ParserParameters
open MainLenses

let inline _argumentInfoExtended_argumentName f = _1 <| f
let inline _argumentInfoExtended_argumentType f = _2 <| f
let inline _argumentInfoExtended_assigner f = _3 <| f
let inline _argumentInfoExtended_argumentInfo f = _4 <| f
let inline _argumentInfoExtended_allowMultipleDefinitions f =
    _argumentInfoExtended_argumentInfo << _allowMultipleDefinitions <| f
let inline _argumentInfoExtended_isMainArgument f =
    _argumentInfoExtended_argumentInfo << _isMainArgument <| f
let inline _argumentInfoExtended_assignmentKey commandInfo f s : Const<_, _> =
    (commandInfo, s ^. _argumentInfoExtended_argumentName) |> f
let makeArgumentInfoExtended (info : PropertyInfo) =
    let t = info.PropertyType

    let isCSNullable = NullabilityInfoContext().Create(info).WriteState = NullabilityState.Nullable
    let isSystemNullable = t.IsGenericType && t.GetGenericTypeDefinition() = typeof<Nullable<_>>.GetGenericTypeDefinition()
    let isOption = t.IsGenericType && t.GetGenericTypeDefinition() = typeof<Option<_>>.GetGenericTypeDefinition()
    let isGenericList = t.IsGenericType && t.GetGenericTypeDefinition() = typeof<List<_>>.GetGenericTypeDefinition()
    let requiredAttributeSet = isNull (info.GetCustomAttribute(typeof<RequiredAttribute>)) = false
    let mainArgumentAttributeSet = isNull (info.GetCustomAttribute(typeof<MainArgumentAttribute>)) = false
    let saveArgument = isNull (info.GetCustomAttribute(typeof<SaveArgumentAttribute>)) = false
    let flag = t = typeof<bool>

    if flag && saveArgument then failwith "Flags cannot be saved at this time." else

    let argumentInfo =
        zero
        |> _argumentType .->
            if flag && not mainArgumentAttributeSet then Flag
            elif not flag && mainArgumentAttributeSet then Main
            elif not flag && not mainArgumentAttributeSet then Regular
            else failwith "Flags cannot be main arguments."
        |> _requirementType .->
            if t = typeof<bool> then Optional else // Flags are always optional
            match info.GetCustomAttribute(typeof<ArgumentRequiredAttribute>) with
            | :? ArgumentRequiredAttribute as x ->
                if requiredAttributeSet then failwith "Required and ArgumentRequired attributes cannot be set at the same time." else
                x.RequirementType
            | _ ->
                if requiredAttributeSet then AlwaysRequired
                elif isCSNullable || isSystemNullable || isOption || isGenericList
                then Optional
                else AlwaysRequired
        |> _description .->
            match info.GetCustomAttribute(typeof<DescriptionAttribute>) with
            | :? DescriptionAttribute as x -> x.Description
            | _ -> ""
        |> _saveArgument .-> saveArgument

    let result = (info.Name, t, (info.SetValue : obj * obj -> unit), argumentInfo)
    if isOption || isSystemNullable then
        // Nullable (or optional) flags are nonsensical
        if t = typeof<bool> then failwith "Flags cannot be nullable or options." else

        let firstGenericArgument = t.GetGenericArguments().[0]
        result
        |> _argumentInfoExtended_argumentType .-> firstGenericArgument
        |> _argumentInfoExtended_assigner .-> (fun (o, v) ->
            let value = t.GetConstructor([|firstGenericArgument|]).Invoke([|v|])
            info.SetValue(o, value)
        )
    elif isCSNullable then result
    elif isGenericList then
        if isCSNullable || isOption then failwith "Generic lists cannot be nullable or options." else

        let firstGenericArgument = t.GetGenericArguments().[0]
        if firstGenericArgument = typeof<bool> then failwith "Flags cannot be defined multiple times, List<bool> is therefore not supported." else

        result
        |> _argumentInfoExtended_argumentType .-> firstGenericArgument
        |> _argumentInfoExtended_assigner .-> (fun (o, v) ->
            let list = info.GetValue(o) :?> System.Collections.IList
            list.Add(v) |> ignore
        )
        |> _argumentInfoExtended_allowMultipleDefinitions .-> true
    elif t.IsGenericType then
        failwith "The only supported generic argument types are F# options, and generic lists (List<T>, ResizeArray-s in F#), which are used to capture arguments that can be specified multiple times."
    else result
let getProperties (t : Type) =
    t.GetProperties(
        BindingFlags.Public
        ||| BindingFlags.DeclaredOnly
        ||| BindingFlags.Instance
        ||| BindingFlags.GetProperty
        ||| BindingFlags.SetProperty
    )
    |> Seq.filter (fun p -> p.CanRead && p.CanWrite)

let inline _commandInfoExtended_commandType f = _1 <| f
let inline _commandInfoExtended_commandInfo f = _2 <| f
let inline _commandInfoExtended_argumentInfosExtended f = _3 <| f
let rec makeCommandInfosExtended relevantTypes results = function
    | [] -> results
    | (currentType : Type, parentInfo)::next ->
        let parentHasMainArgument =
            parentInfo |> Option.map snd |> Option.defaultValue false
        let parent = parentInfo |> Option.map fst
        let argumentInfosExtended =
            getProperties currentType |> Seq.map makeArgumentInfoExtended |> Seq.toList
        let supportedArguments =
            argumentInfosExtended
            |> Seq.map (fun x -> x ^. _argumentInfoExtended_argumentName, x ^. _argumentInfoExtended_argumentInfo)
            |> Map.ofSeq
        let hasMainArgument = argumentInfosExtended |> Seq.exists (view _argumentInfoExtended_isMainArgument)
        if parentHasMainArgument && hasMainArgument then
            failwith "At most one main argument can be defined per command chain."
        let currentCommandInfo = {
            Command =
                if parent |> Option.isNone then ""
                elif currentType.Name.EndsWith("arguments", StringComparison.InvariantCultureIgnoreCase)
                then currentType.Name.Substring(0, currentType.Name.Length - 9)
                else currentType.Name
            SupportedArguments = supportedArguments
            Parent = parent
            Description =
                match currentType.GetCustomAttribute(typeof<DescriptionAttribute>) with
                | :? DescriptionAttribute as x -> x.Description
                | _ -> ""
        }
        let result = (currentType, currentCommandInfo, argumentInfosExtended)
        let children =
            relevantTypes
            |> Seq.filter (fun (t : Type) ->
                t.BaseType = currentType &&
                isNull (t.GetConstructor([||])) = false)
            |> Seq.map (fun t -> t, Some (currentCommandInfo, hasMainArgument))
            |> Seq.toList
        makeCommandInfosExtended relevantTypes (result::results) (children @ next)

let makeState rootType commandInfosExtended =
    let rootCommand =
        (commandInfosExtended
        |> List.find (fun x -> x ^. _commandInfoExtended_commandType = rootType))
        ^. _commandInfoExtended_commandInfo
    let allCommands = commandInfosExtended |> List.map (view _commandInfoExtended_commandInfo)
    let parserState =
        zero
        |> _currentCommand .-> rootCommand
        |> _allCommands .-> allCommands
    parserState
let help (executableName : string) state =
    let sb = StringBuilder()

    let commandPath = state ^. _commandPath
    let commandPathString = String.Join(
        " ",
        commandPath |> Seq.map (fun c -> c.Command.ToLower()) |> Seq.filter (fun s -> s <> ""))
    let currentCommand = state ^. _currentCommand
    let mainArgument =
        currentCommand.SupportedArguments
        |> Seq.tryFind (fun kvp -> kvp.Value ^. _isMainArgument)

    if commandPathString <> "" || mainArgument |> Option.isSome then
        sb.Append($"{executableName.ToLower()}") |> ignore
        sb.Append($" {commandPathString}") |> ignore

        match mainArgument with
        | Some kvp ->
            let name = kvp.Key
            let info = kvp.Value
            let argumentInfoExtended = currentCommand, name, info
            if state ^. _argument_isRequired argumentInfoExtended
            then sb.AppendLine($" <{name.ToLower()}>\n") |> ignore
            else sb.AppendLine($" [<{name.ToLower()}>]\n") |> ignore
        | None -> sb.AppendLine("\n") |> ignore

    if currentCommand.Description <> "" then
        sb.AppendLine(currentCommand.Description).AppendLine("") |> ignore

    let subCommands = state ^. _pendingCommands
    let arguments = state ^. _allSupportedArguments |> Seq.filter (view _argument_isMainArgument >> not)

    let maxLength =
        subCommands |> Seq.map (fun c -> c.Command.Length)
        |> Seq.append (arguments |> Seq.map (fun x -> (x ^._argument_argument).Length + 2)) //Two extra spaces for --
        |> Seq.max

    let titlePrefixLength = 2
    let titleColumnSeparatorLength = 2

    let windowWidth = max (try Console.WindowWidth with _ -> 80) 80
    let titleColumnWidth = maxLength + titlePrefixLength + titleColumnSeparatorLength
    let descriptionColumnWidth = windowWidth - titleColumnWidth

    let printWithWordWrap (title : string) (description : string) =
        let fullTitle =
            title
                .PadLeft(title.Length + titlePrefixLength)
                .PadRight(titleColumnWidth)
        sb.Append(fullTitle) |> ignore

        if description = ""
        then sb.AppendLine("") |> ignore
        elif descriptionColumnWidth < 40
        then sb.AppendLine("").AppendLine(description.PadLeft(description.Length + titleColumnSeparatorLength))|> ignore
        else

        let mutable remainingWidth = descriptionColumnWidth
        for word in description.Split([|' '|]) do
            if word.Length + 1 > remainingWidth then
                sb
                    .AppendLine("")
                    .Append(word.PadLeft(titleColumnWidth + word.Length))
                    .Append(" ")
                |> ignore
                remainingWidth <- descriptionColumnWidth - word.Length - 1
            else
                sb.Append(word).Append(" ") |> ignore
                remainingWidth <- remainingWidth - word.Length - 1
        sb.AppendLine("") |> ignore

    if subCommands |> Seq.isEmpty = false then
        sb.AppendLine("SUBCOMMANDS:\n") |> ignore
        for subCommand in subCommands do
            printWithWordWrap $"{subCommand.Command.ToLower()}" subCommand.Description
        sb.AppendLine("") |> ignore

    let overridesHelp =
        arguments
        |> Seq.exists (fun x -> String.Equals(x ^. _argument_argument, "help", StringComparison.InvariantCultureIgnoreCase))
    let requiredArguments =
        arguments
        |> Seq.filter (fun x -> state ^. _argument_isRequired x)

    if requiredArguments |> Seq.isEmpty = false then
        sb.AppendLine("REQUIRED ARGUMENTS:\n") |> ignore
        for x in requiredArguments do
            printWithWordWrap $"--{(x ^. _argument_argument).ToLower()}" (x ^. _argument_description)
        sb.AppendLine("") |> ignore

    let optionalArguments =
        arguments
        |> Seq.filter (fun x -> state ^. _argument_isRequired x |> not)
    sb.AppendLine("OPTIONAL ARGUMENTS:\n") |> ignore
    for x in optionalArguments do
        printWithWordWrap $"--{(x ^. _argument_argument).ToLower()}" (x ^. _argument_description)
    if not overridesHelp then
        printWithWordWrap "--help" "Prints a list of arguments and commands"

    sb.ToString()
let parser
    (savedArguments : JsonElement)
    (allSupportedArgumentParsers : Map<_, _>) : Parser<unit, ParserState> =

    let eof = eof <?> ""
    let printHelp = updateUserState (_help .-> true) >>. fail ""
    let rec pendingArgumentsParser () = getUserState >>= fun state ->
        let argumentParsers =
            state ^. _pendingArguments true
            |> Seq.map (fun x ->
                let name = x ^. _argument_argument
                if x ^. _argument_isFlag then
                    let parser = skipStringCI $"--{name}"
                    spaces >>. parser .>> spaces
                    >>= (fun () -> updateUserState (_argument_assign x .-> true))
                    <?> $"[--{name.ToLower()}]"
                else
                    let required = state ^. _argument_isRequired x
                    let isMainArgument = x ^. _argument_isMainArgument
                    let assignmentKey = x ^. _argument_assignmentKey
                    let contentParser = allSupportedArgumentParsers[assignmentKey]
                    let captureState (s : CharStream<ParserState>) =
                        Reply(CharStreamState(s))
                    let contentWithRawString =
                        captureState
                        .>>. contentParser
                        .>>. captureState
                        >>= fun ((startS, value), endS) -> fun stream ->
                            let startPosition = startS.GetIndex(stream)
                            let endPosition = endS.GetIndex(stream)
                            let length = endPosition - startPosition
                            stream.BacktrackTo(startS)
                            let rawString = stream.Read(int length)
                            stream.BacktrackTo(endS)
                            Reply((value, rawString.Trim()))
                    let fullArgumentParser =
                        skipStringCI $"--{name}" >>. spaces1 >>. contentWithRawString
                    let finalParser =
                        if isMainArgument then contentWithRawString <|> fullArgumentParser
                        else fullArgumentParser

                    spaces >>. finalParser .>> spaces
                    >>= (fun (value, rawString) ->
                        _argument_assign x .-> value
                        >> _argument_assign_raw x .-> rawString
                        |> updateUserState
                    )
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
    let rec assignSavedArguments state : _ -> ParserState = function
        | [] -> state
        | x:_ * _ * _::xs ->
            let assignmentKey = x ^. _argument_assignmentKey
            let parser = allSupportedArgumentParsers[assignmentKey]

            let rec parseSavedValues state results = function
                | [] -> Result.Ok (results, state)
                | x::xs ->
                    match runParserOnString parser state "" x with
                    | ParserResult.Success (o, state, _) -> parseSavedValues state ((o, x)::results) xs
                    | ParserResult.Failure _ -> Result.Error ()
            let tryApplySavedArgument (json : JsonElement) =
                let argumentName = x ^. _argument_argument
                let present, value = json.TryGetProperty(argumentName)
                if not present then assignSavedArguments state xs else

                let length = value.GetArrayLength()
                let values =
                    seq { for i in 0 .. length - 1 do value[i].GetString() }
                    |> Seq.toList
                    |> parseSavedValues state []
                match values with
                | Result.Ok (vs, newState) ->
                    let assignedState =
                        let folder s (parsed, raw) =
                            s
                            |> _argument_assign x .-> parsed
                            |> _argument_assign_raw x .-> raw
                        List.fold folder  newState vs
                    assignSavedArguments assignedState xs
                | Result.Error _ -> assignSavedArguments state xs
            match x ^. _argument_commandName with
            | "" -> tryApplySavedArgument savedArguments
            | commandName ->
                let present, command = savedArguments.TryGetProperty(commandName)
                if not present then assignSavedArguments state xs else
                tryApplySavedArgument command

    eof >>. printHelp
    <|> pendingArgumentsParser ()
    >>. getUserState >>= fun state ->
        let pending = state ^. _pendingArguments false |> Seq.toList
        let newState = assignSavedArguments state pending
        match newState ^. _missingArguments |> Seq.toList with
        | [] -> setUserState newState
        | missing ->
            let missing =
                missing
                |> Seq.map (fun x ->
                    let missing = x ^. _argument_argument
                    $"--{missing.ToLower()}")
            let missingString = String.concat ", " missing
            fail $"The following required arguments are missing: {missingString}"
let parse parameters : Result<_, _> = monad.strict {
    let relevantTypes = parameters ^. _relevantTypes
    let rootType = parameters ^. _rootType
    let commandInfosExtended = makeCommandInfosExtended relevantTypes [] [(rootType, None)]
    let makeCommandInfoAssignmentMap f =
        commandInfosExtended
        |> Seq.collect (fun cie ->
            let ci = cie ^. _commandInfoExtended_commandInfo
            cie ^. _commandInfoExtended_argumentInfosExtended
            |> Seq.map (fun infoExtended ->
                let assignmentKey = infoExtended ^. _argumentInfoExtended_assignmentKey ci
                (assignmentKey, f infoExtended)
            )
        )
        |> Map.ofSeq
    let allSupportedArgumentParsers =
        makeCommandInfoAssignmentMap <| fun infoExtended ->
            let argumentType = infoExtended ^. _argumentInfoExtended_argumentType
            parameters ^. _contentParser_force argumentType
    let allAssigners = makeCommandInfoAssignmentMap (view _argumentInfoExtended_assigner)
    let initialState = makeState rootType commandInfosExtended
    let args = parameters ^. _arguments
    let savedArguments : JsonElement =
        try JsonSerializer.Deserialize(parameters ^. _savedArguments)
        with _ -> JsonElement()
    let! result =
        match runParserOnString (parser savedArguments allSupportedArgumentParsers) initialState "" args with
        | ParserResult.Success (_, state, _) -> Result.Ok state
        | ParserResult.Failure (_, _, state) when state ^. _help ->
            let executableName = parameters ^. _executableName
            Result.Error (help executableName state)
        | ParserResult.Failure (message, _, _) when message.StartsWith "Error in" ->
            let withoutFirstLine =
                message.Split(Environment.NewLine)
                |> Seq.skip 1
                |> String.concat(Environment.NewLine)
            Result.Error withoutFirstLine
        | ParserResult.Failure (message, _, _) -> Result.Error message
    let assigners =
        result ^. _assignedValues
        |> Seq.collect (fun kvp ->
            let assigner = allAssigners[kvp.Key]
            kvp.Value |> Seq.rev |> Seq.map (fun value -> fun o -> assigner(o, value)))
    let resultCommand = result ^. _currentCommand
    let resultCommandInfoExtended =
        commandInfosExtended
        |> List.tryFind (fun x ->
            x ^. _commandInfoExtended_commandInfo = resultCommand)
    let targetType = resultCommandInfoExtended.Value ^. _commandInfoExtended_commandType
    let target = Activator.CreateInstance targetType
    for assigner in assigners do assigner target

    let argumentsToSave =
        result ^. _valuesToSave
        |> Seq.groupBy (fun kvp -> (fst kvp.Key).Command)
        |> Seq.collect (fun (command, values) -> seq {
            match command with
            | "" ->
                for kvp in values do
                    let array = JsonArray()
                    for value in kvp.Value |> Seq.rev do
                        array.Add(JsonValue.Create(value))
                    yield (kvp.Key |> snd, array :> JsonNode)
            | command ->
                let commandObject = JsonObject()
                for kvp in values do
                    let array = JsonArray()
                    for value in kvp.Value |> Seq.rev do
                        array.Add(JsonValue.Create(value))
                    commandObject.Add(kvp.Key |> snd, array)
                yield (command, commandObject :> JsonNode)
        })
        |> fun nodes ->
            let result = JsonObject()
            for k, v in nodes do
                result.Add(k, v)
            result.ToJsonString(JsonSerializerOptions(WriteIndented=true))

    let targetCasted = target :?> 'a
    let result = targetCasted, argumentsToSave
    return result
}
