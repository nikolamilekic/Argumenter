module Argumenter.Parsers

open System
open FSharpPlus
open FSharpPlus.Lens
open FParsec

open ParserData

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
        return stringArgument
    else
        return! Error $"{t.FullName} is not a supported argument type."
}

let argument name parser =
    skipStringCI $"--{name}" >>. spaces1 >>. parser
    .>> updateUserState (_specifiedArguments %-> Set.add name)
    <?> $"--{name.ToLower()}"
let assign assigner value =
    updateUserState (_assigner %-> (fun a -> a >> assigner value))

let validateRequiredArguments expected =
    getUserState >>= fun state ->
        let specified = state ^. _specifiedArguments
        let missing = Set.difference expected specified
        if Set.isEmpty missing then preturn (state ^. _assigner)
        else
            let missingString = String.concat ", " missing
            fail $"The following required arguments are missing: {missingString}"
let makeParser (argumentInfos : ArgumentInfo seq) : Parser<unit, ParserData> =
    let requiredArguments =
        argumentInfos
        |> Seq.filter (fun info -> info.IsRequired)
        |> Seq.map (fun info -> info.ArgumentName)
        |> Set
    argumentInfos
    |> Seq.map (fun info -> argument info.ArgumentName info.Parser)
    |> choice
    |> many
    >>. (eof <?> "")
    .>> validateRequiredArguments requiredArguments
