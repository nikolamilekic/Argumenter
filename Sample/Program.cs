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


public record Arguments
{
    [MainArgument]
    public string? MainArgument { get; set; }
    public string Required1 { get; set; } = "";
    public string Required2 { get; set; } = "";
    public string? Optional1 { get; set; }

    [RequiredIf(nameof(Required1), "trigger")]
    public string? RequiredIf { get; set; }
}

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
