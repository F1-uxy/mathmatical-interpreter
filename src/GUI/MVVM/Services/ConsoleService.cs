using System;
using GUI;

namespace MathGUI.MVVM;

public class ConsoleService : IConsoleService
{
    private readonly MainViewModel _expressionService;

    public ConsoleService(MainViewModel expression)
    {
        _expressionService = expression;
    }

    public void AppendToConsole(string str, bool marker)
    {
        // Append instead of overwrite
        _expressionService.StatusMessage += marker ? $">> { str}\n" : $"{str}\n";
    }

    public void AppendExceptionToConsole(Exception ex)
    {
        AppendToConsole(ex.Message, false);
    }

    public void AppendExceptionToConsole(string ex)
    {
        AppendToConsole(ex, false);
    }
}