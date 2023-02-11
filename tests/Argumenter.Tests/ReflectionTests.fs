module Argumenter.Tests.ReflectionTests

open Expecto
open Swensen.Unquote
open Argumenter

type RootArguments() =
    member val RequiredString1 = "" with get, set
    member val RequiredString2 = "" with get, set
    member val OptionalString1 : string option = None with get, set
    override _.GetHashCode() = 0
    override this.Equals(other : obj) =
        match other with
        | :? RootArguments as other ->
            this.RequiredString1 = other.RequiredString1 &&
            this.RequiredString2 = other.RequiredString2 &&
            this.OptionalString1 = other.OptionalString1
        | _ -> false


[<Tests>]
let reflectionTests = testList "Reflection" [
    testCase "Good case without optional" <| fun _ ->
        let input = "--requiredString1 value1 --requiredString2 value2"
        let expected = RootArguments(RequiredString1="value1", RequiredString2="value2")
        let actual = ArgumentParser().Parse<RootArguments>(input)
        actual =! Ok expected
    testCase "Good case with optional" <| fun _ ->
        let input = "--requiredString1 value1 --requiredString2 value2 --optionalString1 value3"
        let expected = RootArguments(RequiredString1="value1", RequiredString2="value2", OptionalString1=Some "value3")
        let actual = ArgumentParser().Parse<RootArguments>(input)
        actual =! Ok expected
    testCase "Bad case with missing required" <| fun _ ->
        let input = "--requiredString1 value1 --optionalString1 value3"
        let actual = ArgumentParser().Parse<RootArguments>(input)
        Expect.isError actual "Did not fail when optional arguments are missing"
]

