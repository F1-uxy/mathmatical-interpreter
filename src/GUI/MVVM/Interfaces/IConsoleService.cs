using System;

namespace MathGUI.MVVM;

public interface IConsoleService
{
    void AppendToConsole(string str, bool marker);
    void AppendExceptionToConsole(Exception ex);
    void AppendExceptionToConsole(string ex);
}