using System;
using GUI;

namespace MathGUI.MVVM;

public class ConsoleService : IConsoleService
{
    private readonly MainViewModel _main;

    public ConsoleService(MainViewModel main)
    {
        _main = main;
    }

    public void AppendToConsole(string str, bool marker)
    {
        // Append instead of overwrite
        _main.StatusMessage += marker ? $">> { str}\n" : $"{str}\n";
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