namespace Argumenter

open System

[<AttributeUsage(AttributeTargets.Property)>]
type MainArgumentAttribute() = inherit Attribute()

[<AttributeUsage(AttributeTargets.Property)>]
type RequiredIfAttribute(argumentName : string, value : obj) =
    inherit Attribute()
    member _.ArgumentName = argumentName
    member _.Value = value
