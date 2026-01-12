using System.Runtime;
using MathInterpreter;

namespace InterpreterTests
{
    public class MathematicalInterpreterTests
    {
        public enum ValueType { Int, Float, Complex }

        [TestCase("5*3+(2*3-2)/2+6", 23, ValueType.Int)]
        [TestCase("9-3-2", 4, ValueType.Int)]
        [TestCase("10/3", 3, ValueType.Int)]
        [TestCase("10/3.0", 3.3333, ValueType.Float)]
        [TestCase("10%3", 1, ValueType.Int)]
        [TestCase("10 - -2", 12, ValueType.Int)]
        [TestCase("-2 + 10", 8, ValueType.Int)]
        [TestCase("3*5^(-1+3)-2^2*-3", 87, ValueType.Int)]
        [TestCase("-3^2", 9, ValueType.Int)]
        [TestCase("-7%3", -1, ValueType.Int)]
        [TestCase("2*3^2", 18, ValueType.Int)]
        [TestCase("3*5^(-1+3)-2^-2*-3", 75.750, ValueType.Float)]
        [TestCase("3*5^(-1+3)-2.0^-2*-3", 75.750, ValueType.Float)]
        [TestCase("(((3*2--2)))", 8, ValueType.Int)]
        [TestCase("-((3*5-2*3))", -9, ValueType.Int)]
        [TestCase("x = 3; (2*x)-x^2*5", -39, ValueType.Int)]
        [TestCase("x = 3; (2*x)-x^2*5/2", -16, ValueType.Int)]
        [TestCase("x = 3; (2*x)-x^2*(5/2)", -12, ValueType.Int)]
        [TestCase("x = 3; (2*x)-x^2*5/2.0", -16.5, ValueType.Float)]
        [TestCase("x = 3; (2*x)-x^2*5%2", 5, ValueType.Int)]
        [TestCase("x = 3; (2*x)-x^2*(5%2)", -3, ValueType.Int)]

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
        
        [TestCase("complex(2,3)*-2", -4.0, -6.0, ValueType.Complex)]
        [TestCase("complex(1,-1) + complex(2,3)", 3.0, 2.0, ValueType.Complex)]
        [TestCase("complex(1,-1) * complex(5, 2)", 7.0, -3.0, ValueType.Complex)]
        [TestCase("a = complex(3,4); magnitude(a)", 5.0, 0.0, ValueType.Float)]
        public void ComplexExpressionTests(string expression, double expectedReal, double expectedImag, ValueType type)
        {
            var result = MathInterpreter.interpreter.evaluate(expression);
            switch (type)
            {
                case ValueType.Float:
                    Console.WriteLine(result);
                    var actualFloat = ((interpreter.NumericValue.FloatVal)result).Item;
                    float expectedFloat = (float)expectedReal;
                    Assert.That(actualFloat, Is.EqualTo(expectedFloat).Within(0.0001));
                    break;
                case ValueType.Complex:
                    var complexResult = (interpreter.NumericValue.ComplexVal)result;

                    Assert.That(complexResult.Item1, Is.EqualTo(expectedReal).Within(0.0001), "Real part mismatch");
                    Assert.That(complexResult.Item2, Is.EqualTo(expectedImag).Within(0.0001), "Imag part mismatch");
                    break;
                default:
                    throw new NotSupportedException();
            }
            
        }
    }
}

