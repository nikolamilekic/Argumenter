module Argumenter.MainLenses

open System
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Lens

open ArgumentInfo
open ParserState

let inline _allSupportedArguments f s : Const<_, _> =
    s ^. _commandPath
    |> Seq.rev
    |> Seq.collect (fun c ->
        c.SupportedArguments
        |> Seq.map (fun kvp -> (c, kvp.Key, kvp.Value)))
    |> f

// Source of these lenses is AllSupportedArguments element
let inline _argument_command f = _1 <| f
let inline _argument_argument f = _2 <| f
let inline _argument_argumentInfo f = _3 <| f
let inline _argument_commandName f s : Const<_, _> =
    (s ^. _argument_command).Command |> f
let inline _argument_allowMultipleDefinitions f : _ -> Const<_, _> =
    _argument_argumentInfo << _allowMultipleDefinitions <| f
let inline _argument_isFlag f : _ -> Const<_, _> =
    _argument_argumentInfo << _isFlag <| f
let inline _argument_isMainArgument f : _ -> Const<_, _> =
    _argument_argumentInfo << _isMainArgument <| f
let inline _argument_description f : _ -> Const<_, _> =
    _argument_argumentInfo << _description <| f
let inline _argument_requirementType f : _ -> Const<_, _> =
    _argument_argumentInfo << _requirementType <| f
let inline _argument_assignmentKey f s : Const<_, _> =
    (s ^. _argument_command, s ^. _argument_argument) |> f
let inline _argument_saveArgument f : _ -> Const<_, _> =
    _argument_argumentInfo << _saveArgument <| f
let inline _argument_assign x f s : Identity<_> =
    let assignmentKey = x ^. _argument_assignmentKey
    f ignore <&> fun v ->
        match s.AssignedValues |> Map.tryFind assignmentKey with
        | Some vs ->
            { s with AssignedValues = s.AssignedValues.Add(assignmentKey, v::vs) }
        | _ -> { s with AssignedValues = s.AssignedValues.Add(assignmentKey, [v]) }
let inline _argument_assign_raw x f s : Identity<_> =
    let assignmentKey = x ^. _argument_assignmentKey
    let save = x ^. _argument_saveArgument
    f ignore <&> fun v ->
        if not save then s else

        match s.ValuesToSave |> Map.tryFind assignmentKey with
        | Some ai ->
            { s with ValuesToSave = s.ValuesToSave.Add(assignmentKey, v::ai) }
        | _ -> { s with ValuesToSave = s.ValuesToSave.Add(assignmentKey, [v]) }
let inline _argument_assigned x f s : Const<_, _> =
    let assignmentKey = x ^. _argument_assignmentKey
    s.AssignedValues.TryFind assignmentKey
    |> Option.isSome
    |> f

let inline _argument_isRequired x f s : Const<_, _> =
    match x ^. _argument_requirementType with
    | AlwaysRequired -> true
    | Optional -> false
    | RequiredIf (name, value) when (value :? bool) ->
        //Flags cannot be set multiple times
        let key = (x ^. _argument_assignmentKey) |> _argument_argument .-> name
        value = s.AssignedValues.ContainsKey key
    | RequiredIf (name, value) when (value :? string) ->
        let key = (x ^. _argument_assignmentKey) |> _argument_argument .-> name
        match s.AssignedValues.TryFind key with
        | None -> false
        | Some values ->
            values
            |> List.tryFind (fun v ->
                String.Equals(
                    v :?> string,
                    value :?> string,
                    StringComparison.OrdinalIgnoreCase))
            |> Option.isSome
    | RequiredIf (name, value) ->
        let key = (x ^. _argument_assignmentKey) |> _argument_argument .-> name
        match s.AssignedValues.TryFind key with
        | None -> false
        | Some values -> values |> List.contains value
    |> f
let inline _missingArguments f s : Const<_, _> =
    s ^. _allSupportedArguments
    |> Seq.filter (fun x ->
        let isRequired = s ^. _argument_isRequired x
        let assigned = s ^. _argument_assigned x
        isRequired && not assigned)
    |> f
let inline _pendingArguments includeMultiple f s : Const<_, _> =
    s ^. _allSupportedArguments
    |> Seq.filter (fun x ->
        let allowMultipleSelection = includeMultiple && x ^. _argument_allowMultipleDefinitions
        let assigned = s ^. _argument_assigned x
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
