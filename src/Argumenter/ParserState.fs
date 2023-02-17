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
        RequirementType : ArgumentRequirementType
        Assigner : obj * obj -> unit
        Type : Type
        AllowMultipleDefinitions : bool
        IsMainArgument : bool
        Description : string
    }
    with
    static member Zero = {
        RequirementType = zero
        Assigner = ignore
        Type = typeof<string>
        AllowMultipleDefinitions = false
        IsMainArgument = false
        Description = ""
    }
module ArgumentInfo =
    let inline _requirementType f s =
        s.RequirementType |> f <&> fun v -> { s with RequirementType = v }
    let inline _type f s =
        s.Type |> f <&> fun v -> { s with Type = v }
    let inline _allowMultipleDefinitions f s =
        s.AllowMultipleDefinitions |> f <&> fun v -> { s with AllowMultipleDefinitions = v }
    let inline _assigner f s = s.Assigner |> f <&> fun v -> { s with Assigner = v }
    let inline _isMainArgument f s =
        s.IsMainArgument |> f <&> fun v -> { s with IsMainArgument = v }
    let inline _description f s =
        s.Description |> f <&> fun v -> { s with Description = v }
type CommandInfo =
    {
        Command : string
        Parent : CommandInfo option
        SupportedArguments : Map<string, ArgumentInfo>
        Description : string
    }
type ParserState =
    {
        CurrentCommand : CommandInfo
        AllCommands : CommandInfo list
        AssignedValues : Map<string, ArgumentInfo * obj list>
        Help : bool
    }
    with
    static member Zero = {
        AllCommands = []
        CurrentCommand = {
            Command = ""
            Parent = None
            SupportedArguments = Map.empty
            Description = ""
        }
        AssignedValues = Map.empty
        Help = false
    }

open ArgumentInfo

module ParserState =
    let inline _currentCommand f s =
        s.CurrentCommand |> f <&> fun v -> { s with CurrentCommand = v }
    let inline _allCommands f s =
        s.AllCommands |> f <&> fun v -> { s with AllCommands = v }
    let inline _assigners f s : Const<_, _> =
        s.AssignedValues.Values
        |> Seq.collect (fun (info, values) ->
            values
            |> Seq.rev
            |> Seq.map (fun v o -> (info ^. _assigner)(o, v)))
        |> f
    let inline _help f s =
        s.Help |> f <&> fun v -> { s with Help = v }

    // ArgumentInfo from SupportedArguments KVP
    let inline _argument_allowMultipleDefinitions f : _ -> Const<_, _> =
        _value << _allowMultipleDefinitions <| f
    let inline _argument_type f : _ -> Const<_, _> =
        _value << _type <| f
    let inline _argument_isMainArgument f : _ -> Const<_, _> =
        _value << _isMainArgument <| f
    let inline _argument_description f : _ -> Const<_, _> =
        _value << _description <| f

    let inline _assign kvp f s : Identity<_> =
        let name = kvp ^. _key
        f ignore <&> fun v ->
            match s.AssignedValues |> Map.tryFind name with
            | None ->
                { s with AssignedValues = s.AssignedValues.Add(name, (kvp ^. _value, [v])) }
            | Some (info, values) ->
                { s with AssignedValues = s.AssignedValues.Add(name, (info, v::values)) }
    let inline _assigned name f s : Const<_, _> =
        s.AssignedValues.TryFind name
        |> Option.isSome
        |> f
    let inline _commandPath f s : Const<_, _> =
        let rec parents command = seq {
            yield command
            match command.Parent with
            | None -> ()
            | Some parent -> yield! parents parent
        }
        parents (s ^. _currentCommand) |> Seq.rev |> f
    let inline _allSupportedArguments f s : Const<_, _> =
        s ^. _commandPath
        |> Seq.rev
        |> Seq.collect (fun c -> c.SupportedArguments)
        |> f
    let inline _isRequired argumentInfo f s : Const<_, _> =
        match argumentInfo ^. _requirementType with
        | AlwaysRequired -> true
        | Optional -> false
        | RequiredIf (name, value) when (value :? bool) ->
            //Flags cannot be set multiple times
            value = s.AssignedValues.ContainsKey name
        | RequiredIf (name, value) when (value :? string) ->
            match s.AssignedValues.TryFind name with
            | None -> false
            | Some (_, values) ->
                values
                |> List.tryFind (fun v ->
                    String.Equals(
                        v :?> string,
                        value :?> string,
                        StringComparison.OrdinalIgnoreCase))
                |> Option.isSome
        | RequiredIf (name, value) ->
            match s.AssignedValues.TryFind name with
            | None -> false
            | Some (_, values) -> values |> List.contains value
        |> f
    let inline _missingArguments f s : Const<_, _> =
        s ^. _allSupportedArguments
        |> Seq.filter (fun kvp ->
            let isRequired = s ^. _isRequired (kvp ^. _value)
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
