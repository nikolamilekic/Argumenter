namespace Argumenter

open System
open System.Reflection
open FSharpPlus
open FSharpPlus.Lens
open FParsec

open Parsers
open ArgumentInfo
open ParserState

type ArgumentParser<'a>() =
    let relevantTypes =
        Assembly
            .GetCallingAssembly()
            .GetTypes()
        |> Seq.filter(fun t -> t.IsAssignableTo typeof<'a>)
        |> Seq.toList

    let Ok = Result.Ok
    let Error = Result.Error

    let parser : Parser<unit, ParserState> =
        let rec pendingArgumentsParser () = getUserState >>= fun state ->
            let argumentParsers =
                state ^. _pendingArguments
                |> Seq.map (fun kvp ->
                    let name = kvp ^. _key
                    let required = kvp ^. _argument_isRequired
                    let parser = kvp ^. _argument_parser
                    spaces >>. parser .>> spaces >>= (fun value ->
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
            |> choice
            >>= fun () -> (eof <?> "") <|> pendingArgumentsParser ()
        pendingArgumentsParser () >>. getUserState >>= fun state ->
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
    let getBuiltInParser (t : Type) =
        if t = typeof<string> then
            stringArgument |>> box
        else
            failwith $"{t.Name} is not a supported argument type."
    let getArgumentInfo (info : PropertyInfo) =
        let t = info.PropertyType

        if info.CustomAttributes |> Seq.exists (fun ca -> ca.AttributeType.Name = "NullableAttribute") then
            zero
            |> _parser .-> argument info.Name (getBuiltInParser t)
            |> _assigner .-> info.SetValue
            |> _isRequired .-> false
        elif t.IsGenericType then
            let genericTypeDefinition = t.GetGenericTypeDefinition()
            let firstGenericArgument = t.GetGenericArguments().[0]
            if genericTypeDefinition = typeof<Option<_>>.GetGenericTypeDefinition() then
                zero
                |> _parser .-> argument info.Name (getBuiltInParser firstGenericArgument)
                |> _assigner .-> (fun (o, v) ->
                    let value = t.GetConstructor([|firstGenericArgument|]).Invoke([|v|])
                    info.SetValue(o, value)
                )
                |> _isRequired .-> false
            elif genericTypeDefinition = typeof<System.Collections.Generic.List<_>>.GetGenericTypeDefinition() then
                zero
                |> _parser .-> argument info.Name (getBuiltInParser firstGenericArgument)
                |> _assigner .-> (fun (o, v) ->
                    let list = info.GetValue(o) :?> System.Collections.IList
                    list.Add(v) |> ignore
                )
                |> _isRequired .-> false
                |> _allowMultipleDefinitions .-> true
            else failwith "Currently, the only supported generic arguments are F# options and generic lists (List<T>, ResizeArray-s in F#), which are used to capture arguments which can be specified more than once."
        else
            zero
            |> _parser .-> argument info.Name (getBuiltInParser t)
            |> _assigner .-> info.SetValue
    let rec getCommandInfos results = function
        | [] -> results
        | (currentType, parent)::next ->
            let argumentInfos =
                getProperties currentType
                |> Array.map (fun p -> p.Name, getArgumentInfo p)
                |> Map.ofSeq
            let currentCommandInfo = {
                Command =
                    if currentType.Name.EndsWith("arguments", StringComparison.InvariantCultureIgnoreCase)
                    then currentType.Name.Substring(0, currentType.Name.Length - 9)
                    else currentType.Name
                SupportedArguments = argumentInfos
                Parent = parent
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

    member _.Parse(args : string) : Result<_, _> = monad.strict {
        let parserState =
            zero
            |> _currentCommand .-> rootCommand
            |> _allCommands .-> allCommands
        let! result =
            match runParserOnString parser parserState "" args with
            | ParserResult.Success (_, state, _) -> Ok state
            | ParserResult.Failure (message, _, _) -> Error message
        let resultCommand = result ^. _currentCommand
        let argumentType =
            commandTypes
            |> List.find (fun (c, _) -> LanguagePrimitives.PhysicalEquality c resultCommand)
            |> snd
        let argumentObject = Activator.CreateInstance argumentType
        let assigners = (result ^. _assigners).Values |> Seq.collect Seq.rev
        for assigner in assigners do assigner argumentObject
        return argumentObject :?> 'a
    }
    member this.Parse() : Result<_, _> =
        let firstCommandLineArg = Environment.GetCommandLineArgs()[0]
        let rawArguments = Environment.CommandLine[firstCommandLineArg.Length..].Trim()
        this.Parse(rawArguments)
