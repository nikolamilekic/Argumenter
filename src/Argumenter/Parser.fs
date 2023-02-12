module Argumenter.Parser

open FSharpPlus.Lens
open FParsec

type ParserData =
    {
        Assigner : obj -> obj
        SpecifiedArguments : Set<string>
    }
    with
    static member Zero = {
        Assigner = id
        SpecifiedArguments = Set.empty
    }

let inline _specifiedArguments f s =
    s.SpecifiedArguments |> f <&> fun x -> { s with SpecifiedArguments = x }
let inline _assigner f s =
    s.Assigner |> f <&> fun x -> { s with Assigner = x }

let singleWordString<'a> : Parser<_, 'a> =
    notFollowedByString "--"
    >>. notFollowedByString "\""
    >>. many1CharsTill anyChar (spaces1 <|> eof)
    <?> "single word value"
let multiWordString<'a> : Parser<_, 'a> =
    skipString "\"" >>. many1CharsTill anyChar (skipString "\"")
    <?> "multi word value surrounded by quotes"
let stringArgument<'a> : Parser<_, 'a> = (multiWordString <|> singleWordString)

let argument name parser =
    skipStringCI $"--{name}" >>. spaces1 >>. parser
    .>> updateUserState (_specifiedArguments %-> Set.add name)
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
