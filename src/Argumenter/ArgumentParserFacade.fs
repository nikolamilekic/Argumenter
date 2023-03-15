namespace Argumenter

open System
open FSharpPlus.Lens
open FParsec

open ArgumentParser
open ParserParameters

type ArgumentParser<'a>(parameters) =
    new() = ArgumentParser<'a>(Create<'a> ())

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
        ArgumentParser<'a>(parameters |> _contentParser_force typeof<'arg> .-> (parser |>> box))
    member _.Parse(args : string) : Result<'a, string> =
        parameters
        |> _arguments .-> args
        |> ArgumentParser.parse
    member this.Parse() =
        let firstCommandLineArg = Environment.GetCommandLineArgs()[0]
        let rawArguments = Environment.CommandLine[firstCommandLineArg.Length..].Trim()
        this.Parse(rawArguments)
