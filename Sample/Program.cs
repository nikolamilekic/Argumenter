using System.ComponentModel;
using System.ComponentModel.DataAnnotations;
using Argumenter;

var result = new ArgumentParser<Arguments>().Parse();

if (result.IsOk)
{
    var parsed = result.ResultValue;
    Console.WriteLine(parsed.ToString());
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
    public int Required2 { get; set; } = 0;
    public double? Optional1 { get; set; }

    [RequiredIf(nameof(Required1), "trigger")]
    public string? RequiredIf { get; set; }
}

[Description("Child command description")]
public record ChildCommandArguments : Arguments
{
    public string ChildArgument { get; set; } = "";

    [Required]
    public List<string> Multiple { get; } = new();

    public override string ToString()
    {
        var multiple = String.Join(", ", Multiple);
        return $"{base.ToString()}, {nameof(Multiple)}: {multiple}";
    }
}
