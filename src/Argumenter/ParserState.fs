namespace Argumenter

open System.Collections.Generic
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Lens

[<AutoOpen>]
module LensExtensions =
    let inline _key f (s : KeyValuePair<'k ,'v>) : Const<_, _> = s.Key |> f
    let inline _value f (s : KeyValuePair<'k ,'v>) : Const<_, _> = s.Value |> f

type ParserState =
    {
        SupportedArguments : Map<string, ArgumentInfo>
        Assigners : Map<string, obj -> unit>
    }
    with
    static member Zero = {
        SupportedArguments = Map.empty
        Assigners = Map.empty
    }
and ArgumentInfo =
    {
        IsRequired : bool
        Assigner : obj * obj -> unit
        Parser : FParsec.Primitives.Parser<obj, ParserState>
        AllowMultipleDefinitions : bool
    }
    with
    static member Zero = {
        IsRequired = true
        Assigner = ignore
        Parser = FParsec.Primitives.preturn null
        AllowMultipleDefinitions = false
    }

module ArgumentInfo =
    let inline _isRequired f s =
        s.IsRequired |> f <&> fun v -> { s with IsRequired = v }
    let inline _parser f s =
        s.Parser |> f <&> fun v -> { s with Parser = v }
    let inline _allowMultipleDefinitions f s =
        s.AllowMultipleDefinitions |> f <&> fun v -> { s with AllowMultipleDefinitions = v }
    let inline _assigner f s = s.Assigner |> f <&> fun v -> { s with Assigner = v }

open ArgumentInfo

module ParserState =
    let inline _supportedArguments f s =
        s.SupportedArguments |> f <&> fun v -> { s with SupportedArguments = v }
    let inline _assigners f s : Const<_, _> = s.Assigners |> f

    // ArgumentInfo from SupportedArguments KVP
    let inline _argument_isRequired f : _ -> Const<_, _> =
        _value << _isRequired <| f
    let inline _argument_allowMultipleDefinitions f : _ -> Const<_, _> =
        _value << _allowMultipleDefinitions <| f
    let inline _argument_parser f : _ -> Const<_, _> =
        _value << _parser <| f

    let inline _assign name f s : Identity<_> =
        f ignore <&> fun v ->
            let assigner = s ^. (_supportedArguments << Map._item name << _Some << _assigner)
            { s with Assigners = s.Assigners.Add(name, fun o -> assigner(o, v)) }
    let inline _assigned name f s : Const<_, _> =
        s ^. (_assigners << Map._item name)
        |> Option.isSome
        |> f
    let inline _missingArguments f s : Const<_, _> =
        s.SupportedArguments
        |> Seq.filter (fun sa ->
            let isRequired = sa ^. _argument_isRequired
            let name = sa ^. _key
            let assigned = s ^. _assigned name
            isRequired && not assigned)
        |> Seq.map (view _key)
        |> f
    let inline _pendingArguments f s =
        s.SupportedArguments
        |> Seq.filter (fun sa ->
            let allowMultipleSelection = sa ^. _argument_allowMultipleDefinitions
            let name = sa ^. _key
            let assigned = s ^. _assigned name
            allowMultipleSelection || not assigned
        )
        |> f
