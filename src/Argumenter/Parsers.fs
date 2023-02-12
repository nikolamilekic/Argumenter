module Argumenter.Parsers

open System
open System.Collections.Generic
open FSharpPlus
open FSharpPlus.Lens
open FParsec

open ArgumentInfo

let Ok = Result.Ok
let Error = Result.Error

let singleWordString<'a> : Parser<_, 'a> =
    notFollowedByString "--"
    >>. notFollowedByString "\""
    >>. many1CharsTill anyChar (spaces1 <|> eof)
    <?> "single word value"
let multiWordString<'a> : Parser<_, 'a> =
    skipString "\"" >>. many1CharsTill anyChar (skipString "\"")
    <?> "multi word value surrounded by quotes"
let stringArgument<'a> : Parser<_, 'a> = (multiWordString <|> singleWordString)
let getRawParser (t : Type) : Result<Parser<_, _>, _> = monad.strict {
    if (t = typeof<string>) then
        return stringArgument |>> box
    else
        return! Error $"{t.FullName} is not a supported argument type."
}
let argument (name : string) parser =
    skipStringCI $"--{name}" >>. spaces1 >>. parser
let makeParser expectedArguments : Parser<obj -> obj, _> =
    let inline _key f (s : KeyValuePair<'k ,'v>) = s.Key |> f
    let inline _value f (s : KeyValuePair<'k ,'v>) = s.Value |> f
    let required =
        expectedArguments
        |> Seq.filter (view (_value << _isRequired))
        |> Seq.map (view _key)
        |> Set
    let rec pendingArgumentsParser (providedArguments : Set<_>, composite : obj -> obj) =
        let remover argumentName map =
            let (info : ArgumentInfo) = Map.find argumentName map
            if info ^. _allowMultipleDefinitions = false
            then Map.remove argumentName map
            else map
        let remainingArguments = Seq.foldBack remover providedArguments expectedArguments
        remainingArguments
        |> Seq.map (fun kvp ->
            let (name : string) = kvp.Key
            let required = kvp ^. (_value << _isRequired)
            let assigner = kvp ^. (_value << _assigner)
            let parser = kvp ^. (_value << _parser)
            spaces >>. parser .>> spaces >>= (fun value ->
                (Set.add name providedArguments, composite >> assigner value)
                |> preturn)
            <?> if required then $"--{name.ToLower()}" else $"[--{name.ToLower()}]"
        )
        |> choice
        >>= fun (provided, composite) ->
            let eof = eof >>. preturn (provided, composite)
            let next = pendingArgumentsParser (provided, composite)
            (eof <?> "") <|> next
    pendingArgumentsParser (Set.empty, id) >>= fun (specified, composite) ->
        let missing = Set.difference required specified
        if Set.isEmpty missing then preturn composite else
        let missingString = String.concat ", " missing
        fail $"The following required arguments are missing: {missingString}"
