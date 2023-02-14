namespace Argumenter

open System
open System.Collections.Generic
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Lens

[<AutoOpen>]
module LensExtensions =
    let inline _key f (s : KeyValuePair<'k ,'v>) : Const<_, _> = s.Key |> f
    let inline _value f (s : KeyValuePair<'k ,'v>) : Const<_, _> = s.Value |> f

type ArgumentInfo =
    {
        IsRequired : bool
        Assigner : obj * obj -> unit
        Type : Type
        AllowMultipleDefinitions : bool
        IsMainArgument : bool
    }
    with
    static member Zero = {
        IsRequired = true
        Assigner = ignore
        Type = typeof<string>
        AllowMultipleDefinitions = false
        IsMainArgument = false
    }
module ArgumentInfo =
    let inline _isRequired f s =
        s.IsRequired |> f <&> fun v -> { s with IsRequired = v }
    let inline _type f s =
        s.Type |> f <&> fun v -> { s with Type = v }
    let inline _allowMultipleDefinitions f s =
        s.AllowMultipleDefinitions |> f <&> fun v -> { s with AllowMultipleDefinitions = v }
    let inline _assigner f s = s.Assigner |> f <&> fun v -> { s with Assigner = v }
    let inline _isMainArgument f s =
        s.IsMainArgument |> f <&> fun v -> { s with IsMainArgument = v }
type CommandInfo =
    {
        Command : string
        Parent : CommandInfo option
        SupportedArguments : Map<string, ArgumentInfo>
    }
type ParserState =
    {
        CurrentCommand : CommandInfo
        AllCommands : CommandInfo list
        Assigners : Map<string, (obj -> unit) list>
    }
    with
    static member Zero = {
        AllCommands = []
        CurrentCommand = {
            Command = ""
            Parent = None
            SupportedArguments = Map.empty
        }
        Assigners = Map.empty
    }

open ArgumentInfo

module ParserState =
    let inline _currentCommand f s =
        s.CurrentCommand |> f <&> fun v -> { s with CurrentCommand = v }
    let inline _allCommands f s =
        s.AllCommands |> f <&> fun v -> { s with AllCommands = v }
    let inline _assigners f s : Const<_, _> = s.Assigners |> f

    // ArgumentInfo from SupportedArguments KVP
    let inline _argument_isRequired f : _ -> Const<_, _> =
        _value << _isRequired <| f
    let inline _argument_allowMultipleDefinitions f : _ -> Const<_, _> =
        _value << _allowMultipleDefinitions <| f
    let inline _argument_type f : _ -> Const<_, _> =
        _value << _type <| f
    let inline _argument_assigner f : _ -> Const<_, _> =
        _value << _assigner <| f
    let inline _argument_isMainArgument f : _ -> Const<_, _> =
        _value << _isMainArgument <| f

    let inline _assign kvp f s : Identity<_> =
        let name = kvp ^. _key
        let assigner = kvp ^. _argument_assigner
        f ignore <&> fun v ->
            let assigner o = assigner(o, v)
            match s.Assigners.TryFind name with
            | Some existing -> { s with Assigners = s.Assigners.Add(name, assigner::existing) }
            | None -> { s with Assigners = s.Assigners.Add(name, [assigner]) }
    let inline _assigned name f s : Const<_, _> =
        s ^. (_assigners << Map._item name)
        |> Option.isSome
        |> f
    let inline _allSupportedArguments f s : Const<_, _> =
        let rec getSupportedArguments command = seq {
            yield! command.SupportedArguments
            match command.Parent with
            | None -> ()
            | Some parent -> yield! getSupportedArguments parent
        }
        getSupportedArguments (s ^. _currentCommand) |> f
    let inline _missingArguments f s : Const<_, _> =
        s ^. _allSupportedArguments
        |> Seq.filter (fun kvp ->
            let isRequired = kvp ^. _argument_isRequired
            let name = kvp ^. _key
            let assigned = s ^. _assigned name
            isRequired && not assigned)
        |> Seq.map (view _key >> String.toLower)
        |> f
    let inline _pendingArguments f s : Const<_, _> =
        s ^. _allSupportedArguments
        |> Seq.filter (fun sa ->
            let allowMultipleSelection = sa ^. _argument_allowMultipleDefinitions
            let name = sa ^. _key
            let assigned = s ^. _assigned name
            allowMultipleSelection || not assigned
        )
        |> f
    let inline _pendingCommands f s : Const<_, _> =
        let currentCommand = s ^. _currentCommand
        s ^. _allCommands
        |> Seq.filter (fun c ->
            match c.Parent with
            | Some x when LanguagePrimitives.PhysicalEquality x currentCommand -> true
            | _ -> false)
        |> f
