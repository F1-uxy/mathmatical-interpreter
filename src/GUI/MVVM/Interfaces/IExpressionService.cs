namespace MathGUI.MVVM.Interfaces;

public interface IExpressionService
{
    string InputExpression { get; }
    string StatusMessage { get; set; }
    string CompilerOutput { get; set; }
    string TerminalOutput { get; set; }
}