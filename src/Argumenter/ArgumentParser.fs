﻿namespace Argumenter

open System
open System.Reflection
open FParsec
open FSharpPlus
open FSharpPlus.Lens

open Parsers
open ArgumentInfo
open ParserState

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
            return
                zero
                |> _parser .-> argument info.Name innerParser
                |> _assigner .-> info.SetValue
                |> _isRequired .-> false
        elif t.IsGenericType then
            let genericTypeDefinition = t.GetGenericTypeDefinition()
            let firstGenericArgument = t.GetGenericArguments().[0]
            if genericTypeDefinition = typeof<Option<_>>.GetGenericTypeDefinition() then
                let! innerParser = getRawParser firstGenericArgument
                return
                    zero
                    |> _parser .-> argument info.Name innerParser
                    |> _assigner .-> (fun (o, v) ->
                        let value = t.GetConstructor([|firstGenericArgument|]).Invoke([|v|])
                        info.SetValue(o, value)
                    )
                    |> _isRequired .-> false
            else
                return! Error "The only currently supported generic arguments are options."
        else
            let! parser = getRawParser t
            return
                zero
                |> _parser .-> argument info.Name parser
                |> _assigner .-> info.SetValue
    }

open ArgumentParser

type ArgumentParser() =
    member _.Parse<'a when 'a : (new : unit -> 'a)>(args : string) : Result<_, _> = monad.strict {
        let! argumentInfos =
            getProperties typeof<'a>
            |> Array.map (fun p -> getArgumentInfo p |> Result.map (fun i -> p.Name, i))
            |> sequence
            |> Result.map (fun (infos : (string * ArgumentInfo)[]) -> infos |> Map.ofSeq)
        let parserState =
            zero
            |> _supportedArguments .-> argumentInfos
        let! result =
            match runParserOnString parser parserState "" args with
            | ParserResult.Success (_, state, _) -> Ok state
            | ParserResult.Failure (message, _, _) -> Error message

        let object = new 'a()
        for assigner in (result ^. _assigners).Values do assigner object
        return object
    }
    member this.Parse<'a when 'a : (new : unit -> 'a)>() : Result<_, _> =
        let firstCommandLineArg = Environment.GetCommandLineArgs()[0]
        let rawArguments = Environment.CommandLine[firstCommandLineArg.Length..].Trim()
        this.Parse<'a>(rawArguments)
