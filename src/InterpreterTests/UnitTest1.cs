using MathInterpreter;

namespace InterpreterTests
{
    
    
    public class MathematicalInterpreterTests
    {
        public enum ValueType { Int, Float }

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
    }
}

