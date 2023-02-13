module Argumenter.Parsers

open System
open FSharpPlus
open FSharpPlus.Lens
open FParsec

open ParserState

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
let parser : Parser<unit, ParserState> =
    let rec pendingArgumentsParser () = getUserState >>= fun state ->
        state ^. _pendingArguments
        |> Seq.map (fun kvp ->
            let name = kvp ^. _key
            let required = kvp ^. _argument_isRequired
            let parser = kvp ^. _argument_parser
            spaces >>. parser .>> spaces >>= (fun value ->
                updateUserState (_assign name .-> value))
            <?> if required then $"--{name.ToLower()}" else $"[--{name.ToLower()}]")
        |> choice
        >>= fun () -> (eof <?> "") <|> pendingArgumentsParser ()
    pendingArgumentsParser () >>. getUserState >>= fun state ->
        match state ^. _missingArguments |> Seq.toList with
        | [] -> preturn ()
        | missing ->
            let missingString = String.concat ", " missing
            fail $"The following required arguments are missing: {missingString}"
