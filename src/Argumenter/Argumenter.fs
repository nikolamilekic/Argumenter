namespace Argumenter

open System
open System.Reflection
open FSharpPlus
open FSharpPlus.Lens
open FParsec

open Parser

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
    let private getRawParser (t : Type) : Result<Parser<_, _>, _> = monad.strict {
        if (t = typeof<string>) then
            return stringArgument
        else
            return! Error $"{t.FullName} is not a supported argument type."
    }
    let private optionGenericTypeDefinition =
        typeof<Option<_>>.GetGenericTypeDefinition()

    type ParserInfo = {
        IsRequired : bool
        ArgumentName : string
        Parser : Parser<unit, ParserData>
    }
    let getParserInfo (info : PropertyInfo) = monad.strict {
        let t = info.PropertyType
        if t.IsGenericType then
            if t.GetGenericTypeDefinition() = optionGenericTypeDefinition then
                let innerType = t.GetGenericArguments().[0]
                let! innerParser = getRawParser innerType
                return {
                     IsRequired = false
                     ArgumentName = info.Name
                     Parser = innerParser >>= assign (fun v o -> info.SetValue(o, Some v); o)
                }
            else
                return! Error "Arguments cannot be generic unless they are options."
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
    member this.Parse<'a when 'a : (new : unit -> 'a)>(args : string[]) =
        let singleString = String.concat " " args
        this.Parse<'a>(singleString)
    member _.Parse<'a when 'a : (new : unit -> 'a)>(args : string) : Result<_, _> = monad.strict {
        let! parserInfos =
            getProperties typeof<'a>
            |> Array.map getParserInfo
            |> sequence
        let requiredArguments =
            parserInfos
            |> Seq.filter (fun info -> info.IsRequired)
            |> Seq.map (fun info -> info.ArgumentName)
            |> Set
        let parser =
            parserInfos
            |> Array.map (fun info -> argument info.ArgumentName info.Parser)
            |> choice
            |> many
            .>> validateRequiredArguments requiredArguments
        let! _, data = runParserOnString parser zero "" args |> parserResultToResult
        return (data ^. _assigner) (box (new 'a())) :?> 'a
    }
