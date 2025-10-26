// Simple Interpreter in F#
// Author: R.J. Lapeer 
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

namespace MathInterpreter

module interpreter = 

    open System
    open MathInterpreter.Exceptions
    open Newtonsoft.Json
    type NumericValue =
        IntVal of int | FloatVal of float
    type terminal = 
        Add | Sub | Mul | Div | Mod | Pow | Lpar | Rpar | Comma | Num of NumericValue | Id of string
    
    type EvalResult =
        Number of NumericValue | Plot of X: float[] * Y: float[]

    let str2lst s = [for c in s -> c]
    let isblank c = System.Char.IsWhiteSpace c
    let isdigit c = System.Char.IsDigit c
    let islord c = Char.IsLetterOrDigit c
    let isid c = islord c || c = '_'
    
    let intVal (c:char) = (int)((int)c - (int)'0')
    
    let toFloat = function
        | IntVal i -> float i
        | FloatVal f -> f
    let addNums a b =
        match (a, b) with
        | (IntVal x, IntVal y) -> IntVal ( x + y )
        | _ -> FloatVal (toFloat a + toFloat b)
    let subNums a b=
        match (a, b) with
        | (IntVal x, IntVal y) -> IntVal (x - y)
        | _ -> FloatVal (toFloat a - toFloat b)
    let mulNums a b =
        match (a, b) with
        | (IntVal x, IntVal y) -> IntVal (x * y)
        | _ -> FloatVal (toFloat a * toFloat b)
    
    let divNums a b =
        match (a, b) with
        | (FloatVal 0.0, _) | (IntVal 0, _) -> 
            raise(DivisionByZeroException("Division by zero"))
        | (IntVal x, IntVal y) when x % y = 0 -> IntVal (x / y)
        | _ -> FloatVal (toFloat a / toFloat b)
    
    let modNums a b =
        match (a, b) with
        | (IntVal x, IntVal y) -> IntVal (x % y)
        | _ -> FloatVal (toFloat a % toFloat b)
    let powNums baseVal expVal =
        match (baseVal, expVal) with
        | (IntVal b, IntVal e) when e >= 0 -> 
            IntVal (int (Math.Pow(float b, float e)))
        | _ -> FloatVal (Math.Pow(toFloat baseVal, toFloat expVal))
        
    let negNum a =
        match a with
        | IntVal x -> IntVal (-x)
        | FloatVal x -> FloatVal (-x)

    let rec scFrac (input: char list) (currentValue: float) (place: float) =
        match input with
        | c :: tail when isdigit c ->
            let digitValue = float (intVal c)
            let newValue = currentValue + digitValue * place
            scFrac tail newValue (place/10.0)
        | _ -> (input, currentValue)
        
    let rec scInt(iStr, iVal) = 
        match iStr with
        | '.' :: tail ->
            let (rest, fracValue) = scFrac tail 0.0 0.1
            (rest, Num (FloatVal(float iVal + fracValue )))
        | c :: tail when isdigit c -> scInt(tail, 10*iVal+(intVal c))
        | _ -> (iStr, Num (IntVal iVal))
    
    let rec scId(input, acc) =
        match input with
        | c :: tail when isid c -> scId (tail, acc + string c)
        | _ -> (input, acc)
        
    let knownFunctions : Map<string, NumericValue list -> NumericValue> =
        Map.ofList [
            "sin", (fun args -> 
                match args with
                | [x] -> FloatVal (Math.Round(Math.Sin(toFloat x)))
                | _ -> raise (FunctionArgsException("sin takes 1 argument")))
            "cos", (fun args -> 
                match args with
                | [x] -> FloatVal (Math.Round(Math.Cos(toFloat x)))
                | _ -> raise (FunctionArgsException("cos takes 1 argument")))
            "tan", (fun args -> 
                match args with
                | [x] -> FloatVal (Math.Round(Math.Tan(toFloat x)))
                | _ -> raise (FunctionArgsException("tan takes 1 argument")))
            "abs", (fun args ->
                match args with
                | [IntVal x] -> IntVal (abs x)
                | [FloatVal x] -> FloatVal (abs x)
                | _ -> raise (FunctionArgsException("abs takes 1 argument")))
            "sqrt", (fun args ->
                match args with
                | [x] -> FloatVal (Math.Round(Math.Sqrt(toFloat x)))
                | _ -> raise (FunctionArgsException("sqrt takes 1 argument")))
        ]

    let lexer input = 
        let rec scan input =
            match input with
            | [] -> []
            | '+'::tail -> Add :: scan tail
            | '-'::tail -> Sub :: scan tail
            | '*'::tail -> Mul :: scan tail
            | '%'::tail -> Mod :: scan tail
            | '/'::tail -> Div :: scan tail
            | '^'::tail -> Pow :: scan tail
            | '('::tail -> Lpar:: scan tail
            | ')'::tail -> Rpar:: scan tail
            | ','::tail -> Comma:: scan tail
            | c :: tail when isblank c -> scan tail
            | c :: tail when isdigit c ->
                let (rest, numToken) = scInt(tail, intVal c) 
                numToken :: scan rest
            | '.' :: tail -> 
                let (rest, fracValue) = scFrac tail 0.0 0.1
                Num (FloatVal fracValue) :: scan rest
            | c :: tail when islord c -> let (rest, name) = scId(tail, string c)
                                         Id name :: scan rest
            | _ -> raise (LexerException("Invalid character"))
        scan (str2lst input)

    let getInputString() : string = 
        Console.Write("Enter an expression: ")
        Console.ReadLine()

    // Grammar in BNF:
    // <E>        ::= <T> <Eopt>
    // <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
    // <T>        ::= <NR> <Topt>
    // <Topt>     ::= "%" <NR> <Topt> | "*" <NR> <Topt> | "/" <NR> <Topt> | <empty>
    // <P>        ::= <F> <Popt>
    // <Popt>     ::= "^" <P> | <empty>
    // <F>        ::= <NR> | <FCall> 
    // <NR>       ::= "+" <NR> | "-" <NR> | "Num" <value> | "(" <E> ")"
    // <FCall>    ::= "id" "(" <Args> ")"
    // <Args>     ::= <E> <ArgList> | <empty>
    // <ArgList> ::= "," <E> <ArgList> | <empty>

    let rec parseNeval tList =
        let evalFunc name args =
            match knownFunctions.TryFind(name) with
            | Some f -> f args
            | None -> raise (ParseException($"Unknown function: { name }" ))
        let rec E tList = (T >> Eopt) tList
        and Eopt (tList, value) = 
            match tList with
            | Add :: tail -> let (tLst, tval) = T tail
                             Eopt (tLst, addNums value  tval)
            | Sub :: tail -> let (tLst, tval) = T tail
                             Eopt (tLst, subNums value  tval)
            | _ -> (tList, value)
        and T tList = (P >> Topt) tList
        and Topt (tList, value) =
            match tList with
            | Mul :: tail -> let (tLst, pval) = P tail
                             Topt (tLst, mulNums value  pval)
            | Div :: tail -> let (tLst, pval) = P tail
//                             if pval = 0 then raise(DivisionByZeroException("Division by zero"))
                             Topt (tLst, divNums value  pval)
            | Mod :: tail -> let (tLst, pval) = P tail
                             Topt (tLst, modNums value  pval)
            | _ -> (tList, value)
        and P tList = (NR >> Popt) tList
        and Popt (tList, base_val) =
            match tList with
            | Pow :: tail ->
                let (tLst,exp_val) = P tail
                (tLst, powNums base_val exp_val)
            | _ -> (tList,base_val)
        and NR tList =
            match tList with
            | Add :: tail -> NR tail
            | Sub :: tail ->
                let (tLst, tval) = NR tail
                (tLst, negNum tval)
            | Num value :: tail -> (tail, value)
            | Lpar :: tail -> 
                let (tLst, tval) = E tail
                match tLst with 
                | Rpar :: tail -> (tail, tval)
                | _ -> raise (ParseException("Missing closing parenthesis"))
            | Id name :: Lpar :: tail ->
                let (tLst, args) = parseArgs tail []
                match tLst with
                | Rpar :: rest ->
                    let value = evalFunc name args
                    (rest, value)
                | _ -> raise (ParseException("Missing closing parenthesis"))
            | _ -> raise (ParseException("Unknown NR token"))
        and parseArgs tList acc =
            match tList with
            | Rpar :: _ -> (tList, List.rev acc)  // return the list starting at Rpar
            | _ ->
                let (tLst, tval) = E tList
                parseArgList tLst (tval :: acc)
        and parseArgList tList acc =
            match tList with
            | Comma :: tail ->
                let (tLst, tval) = E tail
                parseArgList tLst (tval :: acc)
            | _ -> (tList, acc)
        let (rest, result) = E tList
        if not rest.IsEmpty then raise (ParseException("Trailing character in parser output")) 
        (rest, result)
    
    let toJson(result: EvalResult) =
        match result with
        | Number n -> JsonConvert.SerializeObject({| ``type`` = "number"; value = n |})    
        | Plot(xs, ys) -> JsonConvert.SerializeObject({| ``type`` = "plot"; x = xs; y = ys |})
        
    // If stepsize does not divide through the range as a whole integer than lexer will fail as floating points not implemented
    let evalPlot (expr: string, xMin: int, xMax: int, stepSize: float) : string =
        let xs = [| for x in seq { float xMin .. stepSize .. float xMax } -> x |]
        let ys = xs |> Array.map ( fun x ->
            let replacement = $"({ x.ToString(System.Globalization.CultureInfo.InvariantCulture) })"
            let substituted = expr.Replace("x", replacement)
            let lexed = lexer substituted
            let (_, result) = parseNeval lexed
            toFloat result
            )
        
        let res = Plot(xs, ys)
        let ret = toJson(res)
        ret
    
    let rec printTList (lst:list<terminal>) : list<string> = 
        match lst with
        head::tail -> Console.Write("{0} ",head.ToString())
                      printTList tail
                      
        | [] -> Console.Write("EOL\n")
                []

    let evaluate(expr: string) : NumericValue =
        let tokens = lexer expr
        let (rest, result) = parseNeval tokens
        result

    [<EntryPoint>]
    let main argv  =
        Console.WriteLine("Simple Interpreter")
        let input:string = getInputString()
        let oList = lexer input
        let pList = printTList (oList)
        let Out = parseNeval oList
        Console.WriteLine("Result = {0}", snd Out)
        0

