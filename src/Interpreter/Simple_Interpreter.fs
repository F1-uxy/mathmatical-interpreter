// Simple Interpreter in F#
// Author: R.J. Lapeer 
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

namespace MathInterpreter

open System.Diagnostics
open System.Text

module interpreter = 

    open System
    open System.IO
    open MathInterpreter.Exceptions
    open Newtonsoft.Json
    type NumericValue =
        IntVal of int | FloatVal of float
    type terminal = 
        Add | Sub | Mul | Div | Mod | Pow | Lpar | Rpar | Comma | Eq | EqEq | GT | LT | Semi | Num of NumericValue | Id of string
    type Expr =
        | Int of NumericValue
        | Binary of Expr * string * Expr
        | Eqiv of Expr * string * Expr
        | Assign of string * Expr
        | Unary of string * Expr
        | Power of Expr * Expr
        | FunCall of string * Expr list
        | Var of string
        | IfExpr of Expr * Expr * Expr option
        | ForLoop of string * Expr * Expr * Expr list
        | Prog of Expr list
    
    type TAC =
        | TACAssign of string * string                   // x := y
        | TACBinary of string * string * string * string // t := y op x
        | TACUnary of string * string * string           // t := op x
        | TACGoto of string
        | TACCall of string * string * string list                // t := call func(args...)
        | TACLabel of string
        | TACIf of string * string                       // if x goto label
        
    type EvalResult =
        Number of NumericValue | Plot of X: float[] * Y: float[]

    let mutable symbTable : Map<string, NumericValue> = Map.empty
    
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
        | (IntVal x, IntVal y) -> IntVal (x / y)
        | _ -> 
            let result = toFloat a / toFloat b
            if result = floor result then
                IntVal (int result)
            else
                FloatVal result
    
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

    let numericEqual (a: NumericValue) (b: NumericValue) =
        match a, b with
        | IntVal x, IntVal y -> x = y
        | FloatVal x, FloatVal y -> x = y
        | IntVal x, FloatVal y -> float x = y
        | FloatVal x, IntVal y -> x = float y
    
    let numericGreaterThan (a: NumericValue) (b: NumericValue) =
        match a, b with
        | IntVal x, IntVal y -> x > y
        | FloatVal x, FloatVal y -> x > y
        | IntVal x, FloatVal y -> float x > y
        | FloatVal x, IntVal y -> x > float y
    
    let numericLessThan (a: NumericValue) (b: NumericValue) =
        match a, b with
        | IntVal x, IntVal y -> x < y
        | FloatVal x, FloatVal y -> x < y
        | IntVal x, FloatVal y -> float x < y
        | FloatVal x, IntVal y -> x < float y

    let rec scFrac (input: char list) (currentValue: float) (place: float) =
        match input with
        | c :: tail when isdigit c ->
            let digitValue = float (intVal c)
            let newValue = currentValue + digitValue * place
            scFrac tail newValue (place/10.0)
        | _ -> (input, currentValue)
    
    let rec scExpDigits input sign acc =
        match input with
        | c :: tail when isdigit c ->
            scExpDigits tail sign (acc * 10 + intVal c)
        | _ -> (input, Some (sign * acc)) 
    
    let rec scExp (input: char list)=
        match input with
        | '+' :: tail -> scExpDigits tail 1 0
        | '-' :: tail -> scExpDigits tail -1 0
        | c :: _ when isdigit c -> scExpDigits input 1 0
        | _ -> (input, None)
            
        
    let rec scInt(iStr, iVal) = 
        match iStr with
        | '.' :: tail -> 
            let (rest, fracValue) = scFrac tail 0.0 0.1
            let baseValue = float iVal + fracValue
            
            match rest with
            | 'e' :: expTail | 'E' :: expTail ->
                let (finalRest, expOpt) = scExp expTail
                match expOpt with
                | Some exp ->
                    (finalRest, Num(FloatVal(baseValue * Math.Pow(10.0, float exp))))
                | None ->
                    (rest, Num (FloatVal baseValue))
            | _ -> (rest, Num (FloatVal baseValue))
        | 'e' :: tail | 'E' :: tail ->
            let (rest, expOpt) = scExp tail
            match expOpt with
            | Some exp ->
                let result = float iVal * Math.Pow(10.0, float exp)
                (rest, Num(FloatVal result) )
            | None ->
                (iStr, Num (IntVal iVal))
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
                | [x] -> FloatVal (Math.Sin(toFloat x))
                | _ -> raise (FunctionArgsException("sin takes 1 argument")))
            "cos", (fun args -> 
                match args with
                | [x] -> FloatVal (Math.Cos(toFloat x))
                | _ -> raise (FunctionArgsException("cos takes 1 argument")))
            "tan", (fun args -> 
                match args with
                | [x] -> FloatVal (Math.Tan(toFloat x))
                | _ -> raise (FunctionArgsException("tan takes 1 argument")))
            "abs", (fun args ->
                match args with
                | [IntVal x] -> IntVal (abs x)
                | [FloatVal x] -> FloatVal (abs x)
                | _ -> raise (FunctionArgsException("abs takes 1 argument")))
            "sqrt", (fun args ->
                match args with
                | [x] -> FloatVal (Math.Sqrt(toFloat x))
                | _ -> raise (FunctionArgsException("sqrt takes 1 argument")))
        ]
    
    let argsToString args =
        let argList = args
                    |> String.concat ","
        argList
    
    let tacToString tac =
        match tac with
        | TACAssign (x, y) -> $"{x} = {y};"
        | TACBinary (t, x, op, y) -> $"{t} = {x} {op} {y};"
        | TACUnary (t, op, x) -> $"{t} = {op} {x};"
        | TACCall (t, funcName, args) -> $"{t} = {funcName}({(argsToString args)});" // We need to differentiate void functions
                                                                            // and assign return value if not void or we assume there are no void functions?
        | _ -> ""

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
            | '='::'='::tail -> EqEq :: scan tail
            | '='::tail -> Eq :: scan tail
            | '>'::tail -> GT :: scan tail
            | '<'::tail -> LT :: scan tail
            | ';'::tail -> Semi :: scan tail
            | c :: tail when isblank c -> scan tail
            | c :: tail when isdigit c ->
                let (rest, numToken) = scInt(tail, intVal c) 
                numToken :: scan rest
            | '.' :: tail -> 
                let (rest, fracValue) = scFrac tail 0.0 0.1
                match rest with
                | 'e' :: expTail | 'E' :: expTail ->
                    let (finalRest,expOpt) = scExp expTail
                    match expOpt with
                    | Some exp ->
                        let value = fracValue * Math.Pow(10.0, float exp)
                        Num (FloatVal value) :: scan finalRest
                    | None ->
                        Num (FloatVal fracValue) :: scan rest
                | _ -> Num (FloatVal fracValue) :: scan rest
            | c :: tail when islord c -> let (rest, name) = scId(tail, string c)
                                         Id name :: scan rest
            | _ -> raise (LexerException("Invalid character"))
        scan (str2lst input)

    let getInputString() : string = 
        Console.Write("Enter an expression: ")
        Console.ReadLine()

    // Grammar in BNF:
    // <Prog>     ::= <S> (";" <S>)*                // Current implementation

    // <Prog>     ::= <S> <Progopt>                 // Fixed implementation
    // <Progopt>  ::= ";" <S> <Progopt> | <empty>
    // <S>        ::= Id "=" <Comp> | <Comp>
    // <Comp>     ::= <E> | "==" <E> | "<" <E> | ">" <E>
    // <E>        ::= <T> <Eopt>
    // <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
    // <T>        ::= <P> <Topt>
    // <Topt>     ::= "%" <NR> <Topt> | "*" <NR> <Topt> | "/" <NR> <Topt> | <empty>
    // <P>        ::= <F> <Popt>
    // <Popt>     ::= "^" <P> | <empty>
    // <F>        ::= <NR> | <FCall> 
    // <NR>       ::= "+" <NR> | "-" <NR> | "Num" <value> | "(" <E> ")" | Id
    // <FCall>    ::= Id "(" <Args> ")"
    // <Args>     ::= <Comp> <ArgList> | <empty>
    // <ArgList> ::= "," <Comp> <ArgList> | <empty>
    
    let rec astEvaluate expr =
        match expr with
        | Prog(statements) ->
            let lastVal =
                statements |> List.fold (fun (lastVal) stmt ->
                    let value = astEvaluate stmt
                    value
                ) (IntVal 0)
            lastVal
        | Int n -> n
        | Unary("-", e) ->
            match astEvaluate e with
            | IntVal n -> IntVal (-n)
            | FloatVal f -> FloatVal (-f)
        | Binary(left, op, right) ->
            let lVal = astEvaluate left
            let rVal = astEvaluate right
            match op with
            | "+" -> addNums lVal rVal
            | "-" -> subNums lVal rVal            
            | "*" -> mulNums lVal rVal
            | "/" -> divNums lVal rVal
            | _ -> raise (ParseException($"Unknown operator: {op}"))
        | Eqiv(left, op, right) ->
            let lVal = astEvaluate left 
            let rVal = astEvaluate right
            match op with
            | "==" -> if numericEqual lVal rVal then IntVal 1 else IntVal 0
            | ">" -> if numericGreaterThan lVal rVal then IntVal 1 else IntVal 0
            | "<" -> if numericLessThan lVal rVal then IntVal 1 else IntVal 0
            | _ -> raise(ParseException($"Unknown comparitor: {op}"))
        | Power(left, right) ->
            powNums (astEvaluate left) (astEvaluate right)
        | Assign(name, expr) ->
            let value = astEvaluate expr
            symbTable <- symbTable.Add(name, value)
            value
        | Var name ->
            match symbTable.TryFind(name) with
            | Some v -> v
            | None -> raise (ParseException($"Unknown variable: {name}"))
        | FunCall(name, args) ->
            let argVals = args |> List.map astEvaluate
            match knownFunctions.TryFind(name) with
            | Some f -> f argVals
            | None -> raise (ParseException($"Unknown function: { name }"))
        | IfExpr(expr1, expr2, exprOption) ->
            let isTrue =
                match astEvaluate expr1 with
                | IntVal n -> n > 0
                | FloatVal f -> f > 0.0
            if isTrue then astEvaluate expr2
            else
                match exprOption with
                | Some expr -> astEvaluate expr
                | None -> IntVal 0
        | ForLoop(varName, startExpr, endExpr, bodyStatements) ->
            let startVal = astEvaluate startExpr
            let endVal = astEvaluate endExpr
            let start = int(toFloat startVal)
            let endNum = int(toFloat endVal)
            
            let rec loop i lastResult =
                if i > endNum then
                    lastResult
                else
                    symbTable <- symbTable.Add(varName, IntVal i)
                    let newResult =
                        bodyStatements
                        |> List.fold (fun _ stmt -> astEvaluate stmt) (IntVal 0)
                    loop (i+1) newResult
            loop start (IntVal 0)
            
        | _ -> raise(ParseException($"Unkown expression: { expr }"))

    let rec parse tList =
        let rec Program tList =
            let rec collect stmts tokens =
                match tokens with
                | [] -> ([], Prog (List.rev stmts))
                | [Semi] -> ([], Prog (List.rev stmts))  // final semicolon
                | Semi :: tail -> collect stmts tail
                | _ ->
                    let (rest, stmt) = S tokens
                    match rest with
                    | [] | [Semi] -> ([], Prog (List.rev (stmt :: stmts)))
                    | Semi :: tail -> collect (stmt :: stmts) tail
                    | _ -> collect (stmt :: stmts) rest
            collect [] tList
            
        and S tList =
            match tList with
            | Id name :: Eq :: tail ->
                let (rest, expr) = Comp tail
                (rest, Assign(name, expr))
            | _ -> Comp tList
        
        and Comp tList =
            let (rest, left) = E tList
            match rest with
            | EqEq :: tail ->
                let (rest2, right) = E tail
                (rest2, Eqiv(left, "==", right))
            | GT :: tail ->
                let (rest2, right) = E tail
                (rest2, Eqiv(left, ">", right))
            | LT :: tail ->
                let (rest2, right) = E tail
                (rest2, Eqiv(left, "<", right))
            | _ -> (rest, left)
        
        and E tList = 
            let (tList', left) = T tList
            Eopt (tList', left)

        and Eopt (tList, left) = 
            match tList with
            | Add :: tail -> 
                let (tList', right) = T tail
                Eopt (tList', Binary(left, "+", right))
            | Sub :: tail -> 
                let (tList', right) = T tail
                Eopt (tList', Binary(left, "-", right))
            | _ -> (tList, left)

        and T tList =
            let (tList', left) = P tList
            Topt (tList', left)

        and Topt (tList, left) =
            match tList with
            | Mul :: tail -> 
                let (tList', right) = P tail
                Topt (tList', Binary(left, "*", right))
            | Div :: tail -> 
                let (tList', right) = P tail
                Topt (tList', Binary(left, "/", right))
            | Mod :: tail -> 
                let (tList', right) = P tail
                Topt (tList', Binary(left, "%", right))
            | _ -> (tList, left)
        
        and P tList =
            let(tList', left) = F tList
            Popt (tList', left)
        
        and Popt (tList, left) =
            match tList with
            | Pow :: tail ->
                let (tList', right) = P tail
                Popt (tList', Power(left, right))
            | _ -> (tList, left)
        
        and F tList =
            match tList with
            | Id "if" :: Lpar :: tail ->
                let (restCond, condExpr) = Comp tail
                match restCond with
                | Rpar :: Id "then" :: thenTail ->
                    let (restThen, thenExpr) = S thenTail
                    match restThen with
                    | Id "else" :: tailElse ->
                        let (restElse, elseExpr) = S tailElse
                        restElse, IfExpr(condExpr, thenExpr, Some elseExpr)
                    | _ -> restThen, IfExpr(condExpr, thenExpr, None)
                | _ -> raise(ParseException("Unknown conditional form"))
                
            | Id "for"  :: Lpar :: Id varName :: Eq :: tail ->
                let (rest1, startExpr) = Comp tail
                match rest1 with
                | Id "to" :: tail2 ->
                    let (rest2, endExpr) = Comp tail2
                    match rest2 with
                    | Rpar :: Id "do" :: tail3 ->
                        let (rest3, bodyStatements) = parseForBody tail3 []
                        match rest3 with
                        | Id "end" :: rest4 ->
                            (rest4, ForLoop(varName, startExpr, endExpr, bodyStatements))
                        | _ -> raise (ParseException"Expected 'end'")
                    | _ -> raise (ParseException "Expected ') do'")
                | _ -> raise (ParseException "Expected 'to'")

            | Id name :: Lpar :: tail ->
                let (rest, args) = Args tail
                match rest with
                | Rpar :: rest' -> (rest', FunCall(name, args))
                | _ -> raise (ParseException "Expected ')' after function call")
                
            | _ -> NR tList

        and NR tList =
            match tList with
            | Sub :: tail ->
                let (rest, expr) = NR tail
                (rest, Unary("-", expr))
            | Lpar :: tail ->
                let (rest, expr) = E tail
                match rest with
                | Rpar :: rest' -> (rest', expr)
                | _ -> raise (ParseException "Expected ')'")
            | Num n :: tail -> (tail, Int(n))
            | Id name :: tail -> (tail, Var(name))
            | _ -> raise (ParseException "Unexpected token in factor")
        
        and Args tList =
            match tList with
            | Rpar :: _ -> (tList, [])
            | _ -> let (tList', first) = E tList
                   ArgList(tList', [first])
        
        and ArgList (tList, acc) =
            match tList with
            | Comma :: tail ->
                let (tList', next) = E tail
                ArgList (tList', next :: acc)
            | _ -> (tList, List.rev acc)
        and parseForBody tokens acc =
            match tokens with
            | Id "end" :: _ -> (tokens, List.rev acc)
            | Semi :: tail -> parseForBody tail acc
            | _ ->
                let (rest, stmt) = S tokens
                parseForBody rest (stmt :: acc)
        match tList with
        | [] -> raise (ParseException "Empty input")
        | _ ->
            let (rest, expr) = Program tList
            (rest, expr)

    let mutable tempCounter = 0
    let assignTemp() =
        tempCounter <- tempCounter + 1
        sprintf "t%A" tempCounter
    
    let declareTemps tac =
        tempCounter <- 0;
        tac
        |> List.choose (function
            | TACAssign(t, _)
            | TACBinary(t, _, _, _)
            | TACUnary(t, _, _) -> Some t
            | TACCall(t, _, _) -> Some t
            | _ -> None
            )
        |> Set.ofList
        |> Set.toList
        |> String.concat ", "
        |> fun s -> $"int {s};"
    
    let rec flattenIRtoTAC ir =
        match ir with
        | Int (IntVal value) ->
            let t = assignTemp()
            [TACAssign(t, string value)], t
        | Int (FloatVal value) ->
            let t = assignTemp()
            [TACAssign(t, string value)], t
        | Var name ->
            [], name
        | Assign(varName, expr) ->
            let (code, t) = flattenIRtoTAC expr
            code @ [TACAssign(varName, t)], varName
        | Unary(op, expr) ->
            let (code, t) = flattenIRtoTAC expr
            let temp = assignTemp()
            code @ [TACUnary(temp, op, t)], temp
        | Binary(x, op, y) ->
            let (codeL, l) = flattenIRtoTAC x
            let (codeR, r) = flattenIRtoTAC y
            let temp = assignTemp()
            codeL @ codeR @ [TACBinary(temp, l, op, r)], temp
        | Power(x, y) ->
            let (codeL, l) = flattenIRtoTAC x
            let (codeR, r) = flattenIRtoTAC y
            let args = [string l; string r]
            let temp = assignTemp()
            codeL @ codeR @ [TACCall(temp, "pow", args)], temp
        | FunCall(funcName, args) ->
            let argTupleList = args |> List.map flattenIRtoTAC
            let argList = argTupleList |> List.collect fst
            let tempList = argTupleList |> List.map snd
            let lastTemp = tempList |> List.last
            let temp = assignTemp()
            argList @ [TACCall(temp, funcName, tempList)], lastTemp
            
        | Prog exprs ->
            let codeList = exprs |> List.map flattenIRtoTAC // Take each line and apply flatten
            let tac = codeList |> List.collect fst          // Take each code block and collect into one list
            let lastVar = codeList |> List.map snd |> List.last // Get last variable assigned which is the final result
            tac, lastVar
        | _ -> raise (ParseException("Unknown IR token during flattening"))
        
    let toJson(result: EvalResult) =
        match result with
        | Number n -> JsonConvert.SerializeObject({| ``type`` = "number"; value = n |})    
        | Plot(xs, ys) -> JsonConvert.SerializeObject({| ``type`` = "plot"; x = xs; y = ys |})
        
    let evalPlot (expr: string, xMin: float, xMax: float, stepSize: float) : string =
        let xs = [| for x in seq { float xMin .. stepSize .. float xMax } -> x |]
        let ys = xs |> Array.map ( fun x ->
            let replacement = $"({ x.ToString(System.Globalization.CultureInfo.InvariantCulture) })"
            let substituted = expr.Replace("x", replacement)
            let lexed = lexer substituted
            let (_, ast) = parse lexed
            let result = astEvaluate ast
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
                
    let printValue = function
    | IntVal x -> string x
    | FloatVal f -> string f

    let writeToFile (fileName : string, str : string) =
        let path = Path.Combine(Path.GetDirectoryName(__SOURCE_DIRECTORY__), "out")
        let result = System.IO.Directory.CreateDirectory(path)
        let path = Path.Combine(path, fileName)
        let file = File.Create(path)
        
        let bytes = System.Text.Encoding.UTF8.GetBytes(str)
        do file.WriteAsync(ReadOnlyMemory bytes) |> ignore
        
        file.Close()
        //File.Delete(path)
    
    let tacString tac =
        let tempDecs = declareTemps tac
        let body = tac
                    |> List.map tacToString
                    |> String.concat "\n"
        $"#include <math.h>\nint main(){{\n{tempDecs}\n\n{body}\nreturn 0;\n}}"
            
    let evaluate(expr: string) : NumericValue =
        let tokens = lexer expr
        let (_, ast) = parse tokens
        let (tac, last) = flattenIRtoTAC ast
        printfn "AST: %A" ast
        printfn "TAC: %A" tac
        printfn "Last: %A" last
        let str = tacString tac
        writeToFile ("tac.c", str)
        let result = astEvaluate ast
        result  
    
    let compile(expr: string) : string =
        let tokens = lexer expr
        let (_, ast) = parse tokens
        let (tac, last) = flattenIRtoTAC ast
        let str = tacString tac
        str
        
    [<EntryPoint>]
    let main argv  =
        Console.WriteLine("Simple Interpreter")
        writeToFile("test.c", "My test string")
        let input:string = getInputString()
        let res = evaluate input
        printfn "Result: %A" res
        printfn "Symbol Table: %A" symbTable
        0

