namespace Argumenter

open FSharpPlus
open FSharpPlus.Lens

type ArgumentInfo =
    {
        IsRequired : bool
        Assigner : obj -> obj -> obj
        Parser : FParsec.Primitives.Parser<obj, unit>
        AllowMultipleDefinitions : bool
    }
    with
    static member Zero = {
        IsRequired = true
        Assigner = konst id
        Parser = FParsec.Primitives.preturn null
        AllowMultipleDefinitions = false
    }

module ArgumentInfo =
    let inline _isRequired f s =
        s.IsRequired |> f <&> fun v -> { s with IsRequired = v }
    let inline _parser f s =
        s.Parser |> f <&> fun v -> { s with Parser = v }
    let inline _allowMultipleDefinitions f s =
        s.AllowMultipleDefinitions |> f <&> fun v -> { s with AllowMultipleDefinitions = v }
    let inline _assigner f s = s.Assigner |> f <&> fun v -> { s with Assigner = v }
