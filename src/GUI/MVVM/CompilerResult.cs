using System;
using Newtonsoft.Json.Linq;

namespace MathGUI.MVVM;

public sealed class CompilerResult
{
    public bool Success { get;}
    public string GeneratedCode { get;}
    public string StdOut { get;}
    public string StdErr { get;}

    private CompilerResult(
        bool success,
        string generatedCode,
        string stdOut,
        string stdErr
    )
    {
        Success = success;
        GeneratedCode = generatedCode;
        StdOut = stdOut;
        StdErr = stdErr;
    }

    public static CompilerResult Ok(
        string generatedCode,
        string stdOut,
        string stdErr
    ) => new CompilerResult(true, generatedCode, stdOut, stdErr);
    
    public static CompilerResult Fail(
        string generatedCode,
        string stdErr
    ) => new CompilerResult(false, generatedCode, "", stdErr);
    
    public static CompilerResult FromGccJson(string json, string code)
    {
        JObject obj = JObject.Parse(json);
        bool success = obj["exit"]?.ToString() == "0";
        string stdout = obj["out"]?.ToString() ?? "";
        string stderr = obj["err"]?.ToString() ?? "";

        return success ? Ok(code, stdout, stderr) : Fail(code, stderr);
    }
    
}