namespace Argumenter

open System
open System.Reflection
open FSharpPlus
open FSharpPlus.Lens
open FParsec

open Parsers
open ParserData

module ArgumentParser =
    let Ok = Result.Ok
    let Error = Result.Error

    let getProperties (t : Type) =
        t.GetProperties(
            BindingFlags.Public
            ||| BindingFlags.Instance
            ||| BindingFlags.GetProperty
            ||| BindingFlags.SetProperty
        )

    let getArgumentInfo (info : PropertyInfo) = monad.strict {
        let t = info.PropertyType
        if info.CustomAttributes |> Seq.exists (fun ca -> ca.AttributeType.Name = "NullableAttribute") then
            let! innerParser = getRawParser t
            return {
                 IsRequired = false
                 ArgumentName = info.Name
                 Parser = innerParser >>= assign (fun v o -> info.SetValue(o, v); o)
            }
        elif t.IsGenericType then
            let genericTypeDefinition = t.GetGenericTypeDefinition()
            let firstGenericArgument = t.GetGenericArguments().[0]
            if genericTypeDefinition = typeof<Option<_>>.GetGenericTypeDefinition() then
                let! innerParser = getRawParser firstGenericArgument
                return {
                     IsRequired = false
                     ArgumentName = info.Name
                     Parser = innerParser >>= assign (fun v o -> info.SetValue(o, Some v); o)
                }
            else
                return! Error "The only currently supported generic arguments are options."
        else
            let! parser = getRawParser t
            return {
                IsRequired = true
                ArgumentName = info.Name
                Parser = parser >>= assign (fun v o -> info.SetValue(o, v); o)
            }
    }
    let parserResultToResult = function
        | ParserResult.Success (result, state, _) -> Ok (result, state)
        | ParserResult.Failure (message, _, _) -> Error message

open ArgumentParser

type ArgumentParser() =
    member this.Parse<'a when 'a : (new : unit -> 'a)>(args : string seq) =
        let singleString = String.concat " " args
        this.Parse<'a>(singleString)
    member _.Parse<'a when 'a : (new : unit -> 'a)>(args : string) : Result<_, _> = monad.strict {
        let! (argumentInfos : ArgumentInfo[]) =
            getProperties typeof<'a>
            |> Array.map getArgumentInfo
            |> sequence
        let parser = makeParser argumentInfos
        let! _, data = runParserOnString parser zero "" args |> parserResultToResult
        return (data ^. _assigner) (box (new 'a())) :?> 'a
    }
