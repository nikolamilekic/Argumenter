namespace Argumenter

open System
open System.Collections.Generic
open System.Collections.Immutable
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Lens
open FParsec

type ParserParameters =
    {
        RootType : Type
        RelevantTypes : Type list
        ExecutableName : string
        ContentParsers : ImmutableDictionary<Type, Parser<obj, ParserState>>
        Arguments : string
    }

module ParserParameters =
    let inline _rootType f s = f s.RootType <&> fun v -> { s with RootType = v }
    let inline _relevantTypes f s = f s.RelevantTypes <&> fun v -> { s with RelevantTypes = v }
    let inline _executableName f s = f s.ExecutableName <&> fun v -> { s with ExecutableName = v }
    let inline _arguments f s = f s.Arguments <&> fun v -> { s with Arguments = v }
    let inline _contentParsers f s = f s.ContentParsers <&> fun v -> { s with ContentParsers = v }
    let inline _contentParser t f s =
        match (s ^. _contentParsers).TryGetValue t with
        | true, x -> f (Some x)
        | _ -> f None
        <&> function
            | Some v -> s |> _contentParsers %-> fun x -> x.SetItem(t, v)
            | None -> s |> _contentParsers %-> fun x -> x.Remove(t)
    let inline _contentParser_force t f s =
        match s ^. _contentParser t with
        | Some x -> f x
        | _ -> failwith $"{t.Name} is not a supported argument type."
        <&> fun v -> s |> _contentParser t .-> Some v

    let singleWordString =
        notFollowedByString "--"
        >>. notFollowedByString "\""
        >>. many1CharsTill anyChar (spaces1 <|> eof)
    let multiWordString =
        skipString "\"" >>. many1CharsTill anyChar (skipString "\"")
    let stringArgument = (multiWordString <|> singleWordString) <?> "string"

    let defaultContentParsers = ImmutableDictionary.Create().AddRange(
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
            typeof<bool>, fail "Bools are handled as flags. This should never be triggered"
            typeof<DateTimeOffset>,
                many1CharsTill anyChar spaces1 >>= fun x ->
                match DateTimeOffset.TryParse x with
                | true, result -> preturn result
                | _ -> fail $"{x} is not a valid date/time."
            typeof<DateTime>,
                many1CharsTill anyChar spaces1 >>= fun x ->
                match DateTime.TryParse x with
                | true, result -> preturn result
                | _ -> fail $"{x} is not a valid date/time."
        ] |> Seq.map KeyValuePair.Create)

    let Create<'a> () =
        let rootType = typeof<'a>
        let assembly = rootType.Assembly
        {
            RootType = rootType
            ExecutableName = assembly.GetName().Name
            ContentParsers = defaultContentParsers
            Arguments = zero
            RelevantTypes =
                let aType = rootType
                let aNamespace = aType.Namespace

                assembly.GetTypes()
                |> Seq.filter(fun t -> t.Namespace = aNamespace && t.IsAssignableTo aType)
                |> Seq.toList
        }
