module Argumenter.Parsers

open FParsec

let singleWordString<'a> : Parser<_, 'a> =
    notFollowedByString "--"
    >>. notFollowedByString "\""
    >>. many1CharsTill anyChar (spaces1 <|> eof)
let multiWordString<'a> : Parser<_, 'a> =
    skipString "\"" >>. many1CharsTill anyChar (skipString "\"")
let stringArgument<'a> : Parser<_, 'a> = (multiWordString <|> singleWordString) <?> "<string>"
