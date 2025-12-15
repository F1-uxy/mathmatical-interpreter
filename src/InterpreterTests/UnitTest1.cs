using System.Diagnostics;
using System.Runtime.CompilerServices;
using MathInterpreter;
using NUnit.Framework;

namespace InterpreterTests
{
    
    
    public class MathematicalInterpreterTests
    {
        public enum ValueType { Int, Float }

        [TestCase("2+2", 4, ValueType.Int)]
        [TestCase("2.5+2.5", 5.0, ValueType.Float)]
        [TestCase("10/3", 3, ValueType.Int)]
        [TestCase("10.0/3.0", 3.3333, ValueType.Float)]
        [TestCase("cos(0)", 1, ValueType.Float)]
        
        public void ExpressionTests(string expression, double expected, ValueType type)
        {
            var result = MathInterpreter.interpreter.evaluate(expression);

            switch (type)
            {
                case ValueType.Int:
                    Assert.That(result, Is.EqualTo(interpreter.NumericValue.NewIntVal((int)expected)));
                    break;
                case ValueType.Float:
                    var actualFloat = ((interpreter.NumericValue.FloatVal)result).Item;
                    float expectedFloat = (float)expected;
                    Assert.That(actualFloat, Is.EqualTo(expectedFloat).Within(0.0001));
                    break;
                default:
                    throw new NotSupportedException();
            }
        }
    }
}

