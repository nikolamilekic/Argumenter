namespace Argumenter

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Lens

type ParserState =
    {
        CurrentCommand : CommandInfo
        AllCommands : CommandInfo list
        AssignedValues : Map<CommandInfo * string, obj list>
        ValuesToSave : Map<CommandInfo * string, string list>
        Help : bool
    }
    with
    static member Zero = {
        AllCommands = []
        CurrentCommand = {
            Command = ""
            Parent = None
            SupportedArguments = Map.empty
            Description = ""
        }
        AssignedValues = Map.empty
        ValuesToSave = Map.empty
        Help = false
    }
and [<StructuralComparison; StructuralEquality>] CommandInfo = {
    Command : string
    Parent : CommandInfo option
    SupportedArguments : Map<string, ArgumentInfo>
    Description : string
}
module ParserState =
    let inline _currentCommand f s =
        s.CurrentCommand |> f <&> fun v -> { s with CurrentCommand = v }
    let inline _allCommands f s =
        s.AllCommands |> f <&> fun v -> { s with AllCommands = v }
    let inline _assignedValues f s =
        s.AssignedValues |> f <&> fun v -> { s with AssignedValues = v }
    let inline _valuesToSave f s =
        s.ValuesToSave |> f <&> fun v -> { s with ValuesToSave = v }
    let inline _help f s =
        s.Help |> f <&> fun v -> { s with Help = v }
    let inline _commandPath f s : Const<_, _> =
        let rec parents command = seq {
            yield command
            match command.Parent with
            | None -> ()
            | Some parent -> yield! parents parent
        }
        parents (s ^. _currentCommand) |> Seq.rev |> f
