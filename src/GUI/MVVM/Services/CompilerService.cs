using System;
using System.CodeDom.Compiler;
using System.Diagnostics;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;

namespace MathGUI.MVVM;

public class CompilerService : ICompilerService
{
    private readonly string _outputDir = "out/";
    private readonly string _path = AppContext.BaseDirectory;
    private readonly string _fileName = "user_code";
    private readonly string _fileExtension = ".c";
    
    public CompilerResult CompileC(string expression)
    {
        string code = MathInterpreter.interpreter.cCompile(expression);
        
        MathInterpreter.interpreter.writeToFile($"{_fileName}{_fileExtension}", code, $"{_path}{_outputDir}");
        string json = MathInterpreter.interpreter.gccCompile(_path, 
            $"{_path}{_outputDir}{_fileName}{_fileExtension}", 
            $"{_path}{_outputDir}{_fileName}");

        return CompilerResult.FromGccJson(json, code);
    }

    public CompilerResult CompileRiscV(string expression)
    {
        string code = MathInterpreter.interpreter.riscvCompile(expression);
        return CompilerResult.Ok(code, string.Empty, string.Empty);
    }

    public CompilerResult RunBinary()
    {
        string binary = $"{_path}{_outputDir}{_fileName}";
        
        var psi = new ProcessStartInfo
        {
            FileName = binary,
            UseShellExecute = false,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            CreateNoWindow = true,
            WorkingDirectory = $"{_path}{_outputDir}"
        };
        
        using var process = Process.Start(psi);

        string stdout = process.StandardOutput.ReadToEnd();
        string stderr = process.StandardError.ReadToEnd();

        process.WaitForExit();
        
        CompilerResult ret = (process.ExitCode == 1)? CompilerResult.Fail("", stderr) 
                                                    : CompilerResult.Ok("", stdout, stderr);
        
        return ret;
    }
}