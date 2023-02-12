using Argumenter;

var rawArguments = Environment.GetCommandLineArgs();
var result = new Argumenter.ArgumentParser()
    .Parse<Arguments>(rawArguments.Skip(1));

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
    public string Required1 { get; set; } = "";
    public string Required2 { get; set; } = "";
    public string? Optional1 { get; set; }
}
