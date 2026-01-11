using System;

namespace MathGUI.MVVM;

public class ComputeService
{
    public string EvaluateExpression(string expression)
    {
        var result = MathInterpreter.interpreter.evaluate(expression);
                
        string resultStr = string.Empty;

        if (result is MathInterpreter.interpreter.NumericValue.IntVal intCase)
            resultStr = intCase.Item.ToString();
        else if (result is MathInterpreter.interpreter.NumericValue.FloatVal floatCase) 
        {
            resultStr = floatCase.Item.ToString("0.0##");
        } else if (result is MathInterpreter.interpreter.NumericValue.ComplexVal complexCase)
        {   
            var real = complexCase.Item1.ToString("0.0##");
            bool isNegative = complexCase.Item2 < 0;
            var imaginary = Math.Abs(complexCase.Item2).ToString("0.0##");
            resultStr = $"{real} {(isNegative ? "-" : "+")} {imaginary}i";
        } else
            resultStr = "Unknown result";

        return resultStr;
    }
}