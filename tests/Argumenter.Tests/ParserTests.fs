module Argumenter.Tests.ParserTests

open Expecto
open FSharpPlus
open Milekic.YoLo
open FParsec
open FsCheck
open Swensen.Unquote

open Argumenter
open Argumenter.Parsers

let Ok = Result.Ok
let Error = Result.Error

let parserResultToResult = function
    | ParserResult.Success (result, _, _) -> Ok result
    | ParserResult.Failure (message, _, _) -> Error message
let parserResultToResultWithState = function
    | ParserResult.Success (result, state, _) -> Ok result, state
    | ParserResult.Failure (message, _, state) -> Error message, state

let runParser p = runParserOnString p zero ""

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
        (runParser singleWordString input
        |> parserResultToResult
        = Ok expected)
    testProperty "multiWordString raw parser" <| fun s ->
        let expected = normalizeSingleLine s
        let input = "\"" + expected + "\""
        let actual = runParser multiWordString input |> parserResultToResult
        let result = actual = Ok expected
        result
    testProperty "singleWordString" <| fun argumentName argument ->
        let input = normalizeSingleWord argument
        let argumentName = normalizeSingleWord argumentName
        let expected = input.Split(' ', '\t', '\n', '\r').[0]
        let parser = Parsers.argument argumentName stringArgument
        let actual =
            runParser parser $"--{argumentName} {input}"
            |> parserResultToResult
        let result = actual = Ok expected
        result
    testProperty "multiWordString" <| fun argumentName argument ->
        let expected = normalizeSingleLine argument
        let input = "\"" + expected + "\""
        let argumentName = normalizeSingleWord argumentName
        let parser = Parsers.argument argumentName stringArgument
        let actual =
            runParser parser $"--{argumentName} {input}"
            |> parserResultToResult
        let result = actual = Ok expected
        result
]

let argumentStateTests = [
    testCase "Parsed arguments are added to the state" <| fun _ ->
        let argumentName = "argumentName"
        let parser = argument argumentName stringArgument
        let actual, state =
            runParser parser $"--{argumentName} argumentValue"
            |> parserResultToResultWithState
        actual =! Ok "argumentValue"
        test <@ state.SpecifiedArguments |> Set.contains argumentName @>
    testCase "Arguments that are not parsed are not added to the state" <| fun _ ->
        let argument1 = argument "argument1" stringArgument
        let argument2 = argument "argument2" stringArgument
        let argument3 = argument "argument3" stringArgument
        let parser = many (choice [argument1; argument2; argument3])
        let result, state =
            runParser parser "--argument1 argumentValue --argument3 argumentValue"
            |> parserResultToResultWithState
        <@
            result |> Result.isOk &&
            state.SpecifiedArguments |> Set.contains "argument1" &&
            state.SpecifiedArguments |> Set.contains "argument3" &&
            not (state.SpecifiedArguments |> Set.contains "argument2")
        @>
        |> test
]

[<Tests>]
let parserTests = testList "Parser" [
    testList "stringArgument" stringArgumentTests
    testList "argumentState" argumentStateTests
]
