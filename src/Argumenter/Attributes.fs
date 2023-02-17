namespace Argumenter

open System
open System.Runtime.InteropServices

[<AttributeUsage(AttributeTargets.Property)>]
type MainArgumentAttribute() = inherit Attribute()

type ArgumentRequirementType =
    | AlwaysRequired
    | Optional
    | RequiredIf of string * obj
    with
    static member Zero = AlwaysRequired

type ArgumentRequiredAttribute(requirementType) =
    inherit Attribute()
    member _.RequirementType = requirementType

    new([<Optional; DefaultParameterValue(true)>] required : bool) =
        ArgumentRequiredAttribute(
            if required
                then ArgumentRequirementType.AlwaysRequired
                else ArgumentRequirementType.Optional
        )
    new(requiredIf, requiredIfValue) =
        ArgumentRequiredAttribute(RequiredIf (requiredIf, requiredIfValue))
