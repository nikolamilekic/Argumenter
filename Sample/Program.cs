using Argumenter;

var result = new Argumenter.ArgumentParser<Arguments>().Parse();

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
}

public record ChildCommandArguments : Arguments
{
    public string ChildArgument { get; set; } = "";
    public List<string> Multiple { get; } = new();
}
