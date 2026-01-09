namespace MathGUI.MVVM;

public interface ICompilerService
{
    CompilerResult CompileC(string code);
    CompilerResult CompileRiscV(string code);
}