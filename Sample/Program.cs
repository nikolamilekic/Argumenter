using System.ComponentModel;
using Argumenter;

//var savedArguments = "{ \"Required2\": [\"2\"], \"ChildCommand\": { \"ChildArgument\": [\"22\"] } }";

var savedArguments = File.Exists("Saved.json") ? File.ReadAllText("Saved.json") : "{}";
var parser = new ArgumentParser<Arguments>().WithSavedArguments(savedArguments);
var result = parser.Parse();

if (result.IsOk)
{
    var parsed = result.ResultValue;
    Console.WriteLine(parsed.ToString());

    Console.WriteLine("SAVED ARGUMENTS");
    Console.WriteLine(parser.ArgumentsToSave);
    File.WriteAllText("Saved.json", parser.ArgumentsToSave);
}
else
{
    Console.WriteLine(result.ErrorValue);
}

[Description("Root description")]
public record Arguments
{
    [MainArgument]
    public string? MainArgument { get; set; }

    [Description("Description of argument called required1")]
    public string Required1 { get; set; } = "";

    [SaveArgument]
    public int Required2 { get; set; } = 0;
    public double? Optional1 { get; set; }
    public bool Flag { get; set; } = false;

    [ArgumentRequired(requiredIf:nameof(Required1), "trigger")]
    public string? RequiredIf { get; set; }
}

[Description("Child command description")]
public record ChildCommandArguments : Arguments
{
    [SaveArgument]
    public string ChildArgument { get; set; } = "";

    [ArgumentRequired, SaveArgument]
    public List<string> Multiple { get; } = new();

    public int NotSaved { get; set; } = 0;

    public override string ToString()
    {
        var multiple = String.Join(", ", Multiple);
        return $"{base.ToString()}, {nameof(Multiple)}: {multiple}";
    }
}
