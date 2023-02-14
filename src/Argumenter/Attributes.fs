namespace Argumenter

open System

[<AttributeUsage(AttributeTargets.Property)>]
type MainArgumentAttribute() = inherit Attribute()
