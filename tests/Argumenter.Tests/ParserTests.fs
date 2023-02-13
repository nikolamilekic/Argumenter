module Argumenter.Tests.ParserTests

open Expecto
open FSharpPlus
open Milekic.YoLo
open FParsec
open FsCheck

open Argumenter
open Parsers

let Ok = Result.Ok
let Error = Result.Error

let parserResultToResult = function
    | ParserResult.Success (result, _, _) -> Ok result
    | ParserResult.Failure (message, _, _) -> Error message
let parserResultToResultWithState = function
    | ParserResult.Success (result, state, _) -> Ok result, state
    | ParserResult.Failure (message, _, state) -> Error message, state

let rec runParser p state = runParserOnString p state ""
let runUnitParser p = runParserOnString p () ""

let normalizeSingleWord (NonEmptyString s) =
    ("a" + s)
        .Trim()
        .Replace("\n", "")
        .Replace("\r", "")
let normalizeSingleLine (NonEmptyString s) =
    s
        .Replace("\"", "x")
        //FParsec automatically new lines to \n
        .Replace("\r\n", "\n")
        .Replace("\r", "\n")

let stringArgumentTests = [
    testProperty "singleWordString raw parser" <| fun s ->
        let input = normalizeSingleWord s
        let expected = input.Split(' ', '\t', '\n', '\r').[0]
        (runUnitParser singleWordString input
        |> parserResultToResult
        = Ok expected)
    testProperty "multiWordString raw parser" <| fun s ->
        let expected = normalizeSingleLine s
        let input = "\"" + expected + "\""
        let actual = runUnitParser multiWordString input |> parserResultToResult
        let result = actual = Ok expected
        result
    testProperty "singleWordString" <| fun argumentName argument ->
        let input = normalizeSingleWord argument
        let argumentName = normalizeSingleWord argumentName
        let expected = input.Split(' ', '\t', '\n', '\r').[0]
        let parser = Parsers.argument argumentName stringArgument
        let actual =
            runUnitParser parser $"--{argumentName} {input}"
            |> parserResultToResult
        let result = actual = Ok expected
        result
    testProperty "multiWordString" <| fun argumentName argument ->
        let expected = normalizeSingleLine argument
        let input = "\"" + expected + "\""
        let argumentName = normalizeSingleWord argumentName
        let parser = Parsers.argument argumentName stringArgument
        let actual =
            runUnitParser parser $"--{argumentName} {input}"
            |> parserResultToResult
        let result = actual = Ok expected
        result
]

[<Tests>]
let parserTests = testList "Parsers" [
    testList "stringArgument" stringArgumentTests
]
