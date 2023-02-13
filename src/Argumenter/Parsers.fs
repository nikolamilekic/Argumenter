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
let getRawParser (t : Type) =
    if t = typeof<string> then
        stringArgument |>> box
    else
        failwith $"{t.Name} is not a supported argument type."
let argument (name : string) parser =
    skipStringCI $"--{name}" >>. spaces1 >>. parser
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
                spaces >>. pstringCI command.Command >>. spaces >>= (fun () ->
                    updateUserState (_currentCommand .-> command))
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
