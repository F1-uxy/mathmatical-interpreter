using System;
using System.CodeDom.Compiler;
using Newtonsoft.Json.Linq;

namespace MathGUI.MVVM;

public class CompilerService : ICompilerService
{
    private readonly string _outputDir = "out/";
    
    public CompilerResult CompileC(string expression)
    {
        string code = MathInterpreter.interpreter.cCompile(expression);
        
        string path = AppContext.BaseDirectory;
        string fileName = "user_code";
        string fileExtension = ".c";
        
        MathInterpreter.interpreter.writeToFile($"{fileName}{fileExtension}", code, $"{path}{_outputDir}");
        string json = MathInterpreter.interpreter.gccCompile(path, 
            $"{path}{_outputDir}{fileName}{fileExtension}", 
            $"{path}{_outputDir}{fileName}");

        return CompilerResult.FromGccJson(json, code);
    }


    public CompilerResult CompileRiscV(string expression)
    {
        string code = MathInterpreter.interpreter.riscvCompile(expression);
        return CompilerResult.Ok(code, string.Empty, string.Empty);
    }
}