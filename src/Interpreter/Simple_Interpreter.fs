// Simple Interpreter in F#
// Author: R.J. Lapeer 
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

namespace MathInterpreter

module interpreter = 

    open System
    open MathInterpreter.Exceptions
    open Newtonsoft.Json
    type terminal = 
        Add | Sub | Mul | Div | Mod | Pow | Lpar | Rpar | Comma | Num of int | Id of string
    
    type EvalResult =
        Number of int | Plot of X: float[] * Y: float[]

    let str2lst s = [for c in s -> c]
    let isblank c = System.Char.IsWhiteSpace c
    let isdigit c = System.Char.IsDigit c
    let islord c = Char.IsLetterOrDigit c
    let isid c = islord c || c = '_'
    
    let intVal (c:char) = (int)((int)c - (int)'0')

    let rec scInt(iStr, iVal) = 
        match iStr with
        c :: tail when isdigit c -> scInt(tail, 10*iVal+(intVal c))
        | _ -> (iStr, iVal)
    
    let rec scId(input, acc) =
        match input with
        | c :: tail when isid c -> scId (tail, acc + string c)
        | _ -> (input, acc)
        
    let knownFunctions : Map<string, int list -> int> =
        Map.ofList [
            "sin", (fun args -> 
                match args with
                | [x] -> int (Math.Round(Math.Sin(float x)))
                | _ -> raise (FunctionArgsException("sin takes 1 argument")))
            "cos", (fun args -> 
                match args with
                | [x] -> int (Math.Round(Math.Cos(float x)))
                | _ -> raise (FunctionArgsException("cos takes 1 argument")))
            "tan", (fun args -> 
                match args with
                | [x] -> int (Math.Round(Math.Tan(float x)))
                | _ -> raise (FunctionArgsException("tan takes 1 argument")))
            "abs", (fun args ->
                match args with
                | [x] -> abs x
                | _ -> raise (FunctionArgsException("abs takes 1 argument")))
            "sqrt", (fun args ->
                match args with
                | [x] -> int (Math.Round(Math.Sqrt(float x)))
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
            | c :: tail when isdigit c -> let (iStr, iVal) = scInt(tail, intVal c) 
                                          Num iVal :: scan iStr
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
        let pown baseVal exp = int (System.Math.Pow(float baseVal, float exp))
        let evalFunc name args =
            match knownFunctions.TryFind(name) with
            | Some f -> f args
            | None -> raise (ParseException($"Unknown function: { name }" ))
        let rec E tList = (T >> Eopt) tList
        and Eopt (tList, value) = 
            match tList with
            | Add :: tail -> let (tLst, tval) = T tail
                             Eopt (tLst, value + tval)
            | Sub :: tail -> let (tLst, tval) = T tail
                             Eopt (tLst, value - tval)
            | _ -> (tList, value)
        and T tList = (P >> Topt) tList
        and Topt (tList, value) =
            match tList with
            | Mul :: tail -> let (tLst, pval) = P tail
                             Topt (tLst, value * pval)
            | Div :: tail -> let (tLst, pval) = P tail
                             if pval = 0 then raise(DivisionByZeroException("Division by zero"))
                             Topt (tLst, value / pval)
            | Mod :: tail -> let (tLst, pval) = P tail
                             Topt (tLst, value % pval)
            | _ -> (tList, value)
        and P tList = (NR >> Popt) tList
        and Popt (tList, base_val) =
            match tList with
            | Pow :: tail ->
                let (tLst,exp_val) = P tail
                (tLst, pown base_val exp_val)
            | _ -> (tList,base_val)
        and NR tList =
            match tList with
            | Add :: tail -> NR tail
            | Sub :: tail ->
                let (tLst, tval) = NR tail
                (tLst, -tval)
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
            float result
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

    let evaluate(expr: string) : int =
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

