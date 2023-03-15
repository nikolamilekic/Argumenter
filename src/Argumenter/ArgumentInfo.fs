namespace Argumenter

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Lens
open Microsoft.FSharp.Core

[<StructuralEquality; StructuralComparison>]
type ArgumentInfo =
    {
        RequirementType : ArgumentRequirementType
        AllowMultipleDefinitions : bool
        ArgumentType : ArgumentType
        Description : string
        SaveArgument : bool
    }
    with
    static member Zero = {
        RequirementType = zero
        AllowMultipleDefinitions = false
        ArgumentType = zero
        Description = ""
        SaveArgument = false
    }
and ArgumentType =
    Regular | Flag | Main
    with static member Zero = Regular

module ArgumentInfo =
    let inline _requirementType f s =
        s.RequirementType |> f <&> fun v -> { s with RequirementType = v }
    let inline _allowMultipleDefinitions f s =
        s.AllowMultipleDefinitions |> f <&> fun v -> { s with AllowMultipleDefinitions = v }
    let inline _argumentType f s = s.ArgumentType |> f <&> fun v -> { s with ArgumentType = v }
    let inline _isMainArgument f s : Const<_, _> = s.ArgumentType = Main |> f
    let inline _isFlag f s = s.ArgumentType = Flag |> f
    let inline _description f s =
        s.Description |> f <&> fun v -> { s with Description = v }
    let inline _saveArgument f s = s.SaveArgument |> f <&> fun v -> { s with SaveArgument = v }
