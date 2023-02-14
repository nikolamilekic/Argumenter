module Argumenter.Tests.ReflectionTests

open System.Linq
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
type ChildArguments() =
    inherit RootArguments()
    member val ChildArgument1 = "" with get, set
    override _.GetHashCode() = 0
    override this.Equals(other : obj) =
        match other with
        | :? ChildArguments as other ->
            base.Equals(other) &&
            this.ChildArgument1 = other.ChildArgument1
        | _ -> false
type MultipleArguments() =
    member val MultipleStringList : ResizeArray<string> = ResizeArray() with get, set
    override _.GetHashCode() = 0
    override this.Equals(other : obj) =
        match other with
        | :? MultipleArguments as other ->
            this.MultipleStringList.SequenceEqual(other.MultipleStringList)
        | _ -> false
type MainArgument() =
    [<MainArgument>]
    member val Main = "" with get, set
    override _.GetHashCode() = 0
    override this.Equals(other : obj) =
        match other with
        | :? MainArgument as other ->
            this.Main = other.Main
        | _ -> false

[<Tests>]
let reflectionTests = testList "Reflection" [
    testCase "Good case without optional" <| fun _ ->
        let input = "--requiredString1 value1 --requiredString2 value2"
        let expected = RootArguments(RequiredString1="value1", RequiredString2="value2")
        let actual = ArgumentParser<RootArguments>().Parse(input)
        actual =! Ok expected
    testCase "Good case with optional" <| fun _ ->
        let input = "--requiredString1 value1 --requiredString2 value2 --optionalString1 value3"
        let expected = RootArguments(RequiredString1="value1", RequiredString2="value2", OptionalString1=Some "value3")
        let actual = ArgumentParser<RootArguments>().Parse(input)
        actual =! Ok expected
    testCase "Bad case with missing required" <| fun _ ->
        let input = "--requiredString1 value1 --optionalString1 value3"
        let actual = ArgumentParser<RootArguments>().Parse(input)
        Expect.isError actual "Did not fail when optional arguments are missing"
    testCase "Regular arguments cannot be added twice" <| fun _ ->
        let input = "--requiredString1 value1 --requiredString1 value2 --requiredString2 value3"
        let actual = ArgumentParser<RootArguments>().Parse(input)
        Expect.isError actual "Did not fail when regular arguments are added twice"
    testCase "Child arguments are parsed correctly" <| fun _ ->
        let input = "--requiredString1 value1 --requiredString2 value2 child --childArgument1 value3"
        let expected = ChildArguments(RequiredString1="value1", RequiredString2="value2", ChildArgument1="value3")
        let actual = ArgumentParser<RootArguments>().Parse(input)
        actual =! Ok expected
    testCase "Multiple arguments are parsed correctly" <| fun _ ->
        let input = "--multiplestringlist value1 --multiplestringlist value2 --multiplestringlist value3"
        let expected = MultipleArguments(MultipleStringList=ResizeArray(["value1"; "value2"; "value3"]))
        let actual = ArgumentParser<MultipleArguments>().Parse(input)
        actual =! Ok expected
    testCase "Main argument is parsed correctly without full name" <| fun _ ->
        let input = "value1"
        let expected = MainArgument(Main="value1")
        let actual = ArgumentParser<MainArgument>().Parse(input)
        actual =! Ok expected
    testCase "Main argument is parsed correctly with full name" <| fun _ ->
        let input = "--main value1"
        let expected = MainArgument(Main="value1")
        let actual = ArgumentParser<MainArgument>().Parse(input)
        actual =! Ok expected
]

