### Mathematical Interpreter Syntax
This document provides some examples and rules of the implemented syntax

#### 1.0 Basic Arithmetic Functions
A program is made up of a list of statements seperated by semi-colons ';'

Basic functions of the interpreter include: 
<ul>
    <li> Unary signals: -x
    <li> Numeric operations: +, -, /, *, %
    <li> Comparison operators: >, <, ==
    <li> Powers: ^
</ul>

Boolean values are represented internally by 1 & 0. Values greater than 0 are true, values 

An example program is as follows:
```
2 + 2;  // 4
4 / 2;  // 2
5 * 5;  // 25
4 % 2;  // 0
3 ^ 2;  // 9
-2 + 4; // 2
3 == 3; // 1
4 < 1   // 0
```
#### 2.0 Variable Assignment
The assigment operator = is used to evaluate and assign a value to an identifier.

The language implemented is a dynamically, weakly typed language similar to Perl. Variables are key value pairs stored in a mutable symbol table. Variables can be assigned and re-assigned to another data type at any point during the program without explicit casting.

An example valid program:
```
r = 2;
pi = 3.14;
area = r * pi; // r is dynamically promoted to a floating point value during interpretation
// Area = 6.28

r = 2.5;
area = r * pi;
// Area = 7.85
```

#### 3.0 Control Flow
There are multiple methods of controlling the flow of the program:

##### 3.1 If Then Else
The If Then Else statement is made up of 2 compulsory components (If & Then) and an optional else statement. The conditions are evaluated under the boolean conditions mentioned in Section 1.0.
```
x = 0;
y = 0;
if(2 == 2) then {
    x = 1;      
    y = x * 5;  
}
// Result:
// x = 1
// y = 5
```

Conditional blocks can be stacked:
```
if(2 == 3) then {
    x = 1; 
    y = 5;
} else {
    if(2 == 2) then {
        x = 3; 
        y = 3;
    }
}
// Result:
// x = 3
// y = 5
```

##### 3.2 For Loop
For loops run for a defined duration. Iterator variables are persistant in the symbol table after use.

```
x = 0
for(i = 1 to 10) do {
    x = x + 1;
}
// Result
// x = 10
// i = 10
```


##### 3.3 While Loop
Unlike for loops, while loops have no bounds. There is no safety mechanism for runaway while loops so the user must implement a correct stopping condition.

```
i = 0
while(i < 10) do {
    i = i + 1;
}
// Result
// i = 10
```

#### 4.0 Advanced Arithmetic Functions
More advanced arithmetic features have been implemented to represent floating point and complex values and expand the parser understanding to scientific notation

##### 4.1 Floating Point 
Floating point data types can be used to store decimal values. Their creation are implicit and if involved with integer data types, all values are promoted to floating type.

```
pi = 3.14;
r = 2;
area = r * pi;
// Result:
// area = FloatVal 6.28
// pi = FloatVal 3.14
// r = IntVal 2
```

##### 4.2 Scientific Notation
Scientific notation simplifies the representation of exponentially large/small numbers. The notation is recognised by the parser and converted into a floating point value. Fractional scales can be used as seen in the example below.
```
x = 3e5;
y = 2.1e-2;
z = 2e1/2
// Result:
// x = FloatVal 300000.0
// y = FloatVal 0.0210
// z = FloatVal 10
```

##### 4.3 Complex Numbers
Imaginary values can be represented using the complex function and accompanying helper functions. complex(r,imag) create a ComplexVal data type which can be used in calculations. The real and imaginary parts can be accessed using the real(x) or imag(x) functions
```
a = complex(3, 4);
b = complex(1, -2);

c = a * b;
// Result:
// c = ComplexVal(11.0, -2)
```

The helper functions provided are magnitude and conjugate and take a ComplexVal as a single argument.
```
a = complex(3, 4);
b = magnitude(a);
c = conjugate(a);
// Result:
// b = FloatVal 5.0
// c = ComplexVal(3.0, -4.0)
```
#### 5.0 Built-in Functions
Built-in functions use standard function call syntax:
```
sin(x)
cos(x)
tan(x)
abs(x)
sqrt(x)
print(x)
```

Examples:
```
x = sin(1.57);
y = abs(-5);
print(42);
```

#### 6.0 User-Defined Functions
Function definition syntax:
```
func <name>(<param1>, <param2>, ...) {
    <statements>;
    return <expression>;
}
```

Examples:
```
func add(a, b) {
    return a + b;
}

func factorial(n) {
    if(n < 2) then {
        return 1;
    } else {
        return n * factorial(n - 1);
    }
}

x = add(5, 3);
y = factorial(5);
```