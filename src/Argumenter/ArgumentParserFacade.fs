namespace Argumenter

open System
open FSharpPlus.Lens
open FParsec

open ArgumentParser
open ParserParameters

type ArgumentParser<'a>(parameters) =
    new() = ArgumentParser<'a>(Create<'a> ())

    member val ArgumentsToSave = "" with get, set

    member _.Help () =
        let executableName = parameters ^. _executableName
        let relevantTypes = parameters ^. _relevantTypes
        let rootType = parameters ^. _rootType
        let commandInfosExtended = makeCommandInfosExtended relevantTypes [] [(rootType, None)]
        let parserState = makeState rootType commandInfosExtended
        help executableName parserState
    member _.WithExecutableName(name : string) =
        ArgumentParser<'a>(parameters |> _executableName .-> name)
    member _.WithCustomParser<'arg>(parser : Parser<'arg, _>) =
        ArgumentParser<'a>(parameters |> _contentParser typeof<'arg> .-> Some (parser |>> box))
    member _.WithSavedArguments(savedArguments : string) =
        ArgumentParser<'a>(parameters |> _savedArguments .-> savedArguments)
    member this.Parse(args : string) : Result<'a, string> =
        (parameters
        |> _arguments .-> args
        |> parse)
        |> Result.map (fun (result, argumentsToSave) ->
            this.ArgumentsToSave <- argumentsToSave
            result)
    member this.Parse(args : string[]) : Result<'a, string> =
        let args =
            args
            |> Seq.map(fun x ->
                if x.StartsWith("\"") && x.EndsWith("\"") then x
                elif x.Contains(' ') then $"\"{x}\""
                else x)
        let argsString = String.Join(" ", args)
        this.Parse(argsString)
    member this.Parse() = this.Parse(Environment.GetCommandLineArgs() |> Array.skip 1)
