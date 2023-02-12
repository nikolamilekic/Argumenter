namespace Argumenter

open FParsec
open FSharpPlus.Lens

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
and ArgumentInfo = {
    IsRequired : bool
    ArgumentName : string
    Parser : Parser<unit, ParserData>
}

module ParserData =
    let inline _specifiedArguments f s =
        s.SpecifiedArguments |> f <&> fun x -> { s with SpecifiedArguments = x }
    let inline _assigner f s =
        s.Assigner |> f <&> fun x -> { s with Assigner = x }

module ArgumentInfo =
    let inline _isRequired f s =
        s.IsRequired |> f <&> fun v -> { s with IsRequired = v }
    let inline _argumentName f s =
        s.ArgumentName |> f <&> fun v -> { s with ArgumentName = v }
    let inline _parser f s =
        s.Parser |> f <&> fun v -> { s with Parser = v }
