namespace Argumenter

open System

[<CustomEquality; CustomComparison>]
type ArgumentRequirementType =
    | AlwaysRequired
    | Optional
    | RequiredIf of string * obj
    with
    static member Zero = AlwaysRequired
    member this.GetRank() =
        match this with
        | AlwaysRequired -> 0
        | Optional -> 1
        | RequiredIf _ -> 2
    override this.Equals(other) =
        match other with
        | :? ArgumentRequirementType as other -> this.Equals(other)
        | _ -> false
    member this.Equals(other : ArgumentRequirementType) =
        match this, other with
        | AlwaysRequired, AlwaysRequired -> true
        | Optional, Optional -> true
        | RequiredIf (leftName, leftValue), RequiredIf (rightName, rightValue) ->
            leftName = rightName && leftValue = rightValue
        | _ -> false
    override this.GetHashCode() =
        let hc = HashCode()
        hc.Add(this.GetRank())
        match this with
        | RequiredIf (name, value) ->
            hc.Add(name)
            hc.Add(value)
        | _ -> ()
        hc.ToHashCode()
    interface IComparable with
        member this.CompareTo(obj) = this.GetHashCode().CompareTo(obj.GetHashCode())
