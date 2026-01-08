// Simple Interpreter in F#
// Author: R.J. Lapeer 
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

namespace MathInterpreter

open System.Collections.Generic
open System.Diagnostics
open System.Linq
open System.Text
open MathInterpreter
open System.Text

module interpreter = 

    open System
    open System.IO
    open MathInterpreter.Exceptions
    open Newtonsoft.Json
    type NumericValue =
        | IntVal of int
        | FloatVal of float
        | ComplexVal of float * float
    type terminal = 
        Add | Sub | Mul | Div | Mod | Pow | Lpar | Rpar | Lcurl | Rcurl | Comma | Eq | EqEq | GT | LT | Semi | Num of NumericValue | Id of string
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
        | WhileLoop of Expr * Expr list
        | Prog of Expr list
    
    type Operand =
        | OpImmInt of int
        | OpImmFloat of float
        | OpVar of string
        | OpTemp of int
        
    type TAC =
        | TACAssign of Operand * Operand                     // x := y
        | TACBinary of Operand * Operand * string * Operand  // t := y op x
        | TACUnary of Operand * string * Operand             // t := op x
        | TACGoto of string
        | TACCall of Operand * string * Operand list          // t := call func(args...)
        | TACLabel of string
        | TACIf of Operand * string                          // if x goto label
        | TACEquiv of Operand * Operand * string * Operand     // t := x op y
        | TACPrint of Operand
        
    type EvalResult =
        Number of NumericValue | Plot of X: float[] * Y: float[]
    
    type BasicBlock =
        {
            id: int
            instrs: TAC list
            mutable succs: int list
            mutable preds: int list
        }

    let mutable symbTable : Map<string, NumericValue> = Map.empty
   
    let tempRegs = [ "$t0"; "$t1"; "$t2"; "$t3"; "$t4"; "$t5"; "$t6"; "$t7"; "$t8"; "$t9"]
    let mutable freeRegs = tempRegs
    let tempMap = Dictionary<int, string>()
    
    let str2lst s = [for c in s -> c]
    let isblank c = System.Char.IsWhiteSpace c
    let isdigit c = System.Char.IsDigit c
    let islord c = Char.IsLetterOrDigit c
    let isid c = islord c || c = '_'
    
    let intVal (c:char) = (int)((int)c - (int)'0')
    
    let toFloat = function
        | IntVal i -> float i
        | FloatVal f -> f
    
    let toComplex = function
        | IntVal i -> (float i, 0.0)
        | FloatVal f -> (float f, 0.0)
        | ComplexVal (r, i) -> (r, i)
    
    let isComplex = function
        | ComplexVal _ -> true
        | _ -> false
    
    let complexToString (r, i) =
        if i = 0.0 then string r
        elif r = 0.0 then $"{i}i"
        elif i > 0.0 then $"{r}+{i}i"
        else $"{r}{i}i"
        
        
    
    let addComplex (r1, i1) (r2, i2) =
        ComplexVal (r1 + r2, i1 + i2)
    
    let subComplex (r1, i1) (r2, i2) =
        ComplexVal (r1 - r2, i1 - i2)
        
    let mulComplex (r1, i1) (r2, i2) =
        let real = r1 * r2 - i1 * i2
        let imag = r1 * i2 + i1 * r2
        ComplexVal (real, imag)

    let divComplex (r1, i1) (r2, i2) =
        if r2 = 0.0 && i2 = 0.0 then
            raise(DivisionByZeroException("Division by zero"))
        let denom = r2 * r2 + i2 * i2
        let real = (r1 * r2 + i1 * i2) / denom
        let imag = (i1 * r2 - r1 * i2) / denom
        ComplexVal (real, imag)
        
    let addNums a b =
        match (a, b) with
        | (IntVal x, IntVal y) -> IntVal ( x + y )
        | (ComplexVal _, _) | (_, ComplexVal _) -> 
            addComplex (toComplex a) (toComplex b)
        | _ -> FloatVal (toFloat a + toFloat b)
    
    let subNums a b=
        match (a, b) with
        | (IntVal x, IntVal y) -> IntVal (x - y)
        | (ComplexVal _, _) | (_, ComplexVal _) -> 
            subComplex (toComplex a) (toComplex b)
        | _ -> FloatVal (toFloat a - toFloat b)

    let mulNums a b =
        match (a, b) with
        | (IntVal x, IntVal y) -> IntVal (x * y)
        | (ComplexVal _, _) | (_, ComplexVal _) -> 
            mulComplex (toComplex a) (toComplex b)
        | _ -> FloatVal (toFloat a * toFloat b)

    let divNums a b =
        match (a, b) with
        | (FloatVal 0.0, _) | (IntVal 0, _) -> 
            raise(DivisionByZeroException("Division by zero"))
        | (IntVal x, IntVal y) -> IntVal (x / y)
        | (ComplexVal _, _) | (_, ComplexVal _) -> 
            divComplex (toComplex a) (toComplex b)
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
        | ComplexVal (r1, i1), ComplexVal (r2, i2) -> r1 = r2 && i1 = i2
        | ComplexVal (r, i), _ when i = 0.0 -> r = toFloat b
        | _, ComplexVal (r, i) when i = 0.0 -> toFloat a = r
        | _ -> false
        
    let numericGreaterThan (a: NumericValue) (b: NumericValue) =
        match a, b with
        | ComplexVal _, _ | _, ComplexVal _ -> 
            raise (ParseException("Cannot compare complex numbers with > or <"))
        | IntVal x, IntVal y -> x > y
        | FloatVal x, FloatVal y -> x > y
        | IntVal x, FloatVal y -> float x > y
        | FloatVal x, IntVal y -> x > float y
    
    let numericLessThan (a: NumericValue) (b: NumericValue) =
        match a, b with
        | ComplexVal _, _ | _, ComplexVal _ -> 
            raise (ParseException("Cannot compare complex numbers with > or <"))
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
        
    let printValue = function
        | IntVal x -> string x
        | FloatVal f -> string f
        | ComplexVal (r, i) -> complexToString (r, i)
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
            "complex", (fun args ->
                match args with
                | [IntVal r; IntVal i] -> ComplexVal (float r, float i)
                | [FloatVal r; FloatVal i] -> ComplexVal (r, i)
                | [IntVal r; FloatVal i] -> ComplexVal (float r, i)
                | [FloatVal r; IntVal i] -> ComplexVal (r, float i)
                | _ -> raise (FunctionArgsException("complex takes 2 arguments (real, imaginary)")))
            "real", (fun args ->
                match args with
                | [ComplexVal (r, _)] -> FloatVal r
                | [IntVal x] -> IntVal x
                | [FloatVal f] -> FloatVal f
                | _ -> raise (FunctionArgsException("real takes 1 argument")))
            "imag", (fun args ->
                match args with
                | [ComplexVal (_, i)] -> FloatVal i
                | [IntVal _] -> IntVal 0
                | [FloatVal _] -> FloatVal 0.0
                | _ -> raise (FunctionArgsException("imag takes 1 argument")))
            "magnitude", (fun args ->
                match args with
                | [ComplexVal (r, i)] -> FloatVal (sqrt(r * r + i * i))
                | [IntVal x] -> FloatVal (abs (float x))
                | [FloatVal f] -> FloatVal (abs f)
                | _ -> raise (FunctionArgsException("magnitude takes 1 argument")))
            "conjugate", (fun args ->
                match args with
                | [ComplexVal (r, i)] -> ComplexVal (r, -i)
                | [x] -> x
                | _ -> raise (FunctionArgsException("conjugate takes 1 argument")))
            "print", (fun args -> 
                match args with
                | [x] -> 
                    printfn "%s" (printValue x)  // Prints to console
                    IntVal 0  // Return 0 (like void in C)
                | _ -> raise (FunctionArgsException("print takes 1 argument")))
        ]
  
    let operandToString operand =
        match operand with
        | OpVar x -> string x
        | OpTemp x -> $"t{x}"
        | OpImmInt x -> string x
        | OpImmFloat x -> string x
        
    let operandListToString args =
        let argList = args
                    |> List.map operandToString 
                    |> String.concat ","
        argList
    let isRealOperand op =
        match op with
        | OpImmInt _
        | OpImmFloat _ -> false
        | _ -> true
        
    let collectDefs tacList : Operand list =
        tacList |> List.collect ( function
            | TACAssign (dst, _) -> [dst]
            | TACUnary (dst, _, _) -> [dst]
            | TACBinary (dst, _, _, _) -> [dst]
            | TACEquiv (dst, _, _, _) -> [dst]
            | TACCall (dst, _, _) -> [dst]
            | _ -> []
        )
        |> List.filter isRealOperand
        |> List.distinct
        
    let collectUses tacList : Operand list =
        tacList |> List.collect (function
            | TACAssign (_, src) -> [src]
            | TACUnary (_, _, src) -> [src]
            | TACBinary (_, src1, _, src2) -> [src1; src2]
            | TACEquiv (_, src1, _, src2) -> [src1; src2]
            | TACIf (x, _) -> [x]
            | TACCall (_, _, args) -> args
            | TACPrint arg -> [arg]
            | TACGoto _
            | TACLabel _ -> []
        )
        |> List.filter isRealOperand
        |> List.distinct
    
    let collectOperands tacList : Operand list =
        tacList |> List.collect ( function
            | TACAssign (dst, src) -> [dst; src]
            | TACUnary(dst, _, src) ->
                [dst; src]
            | TACBinary(dst, src1, _, src2) -> [dst; src1; src2]
            | TACEquiv(dst, src1, _, src2) -> [dst; src1; src2]
            | TACIf(x, _) -> [x]
            | TACCall(dst, _, args) -> dst :: args
            | TACPrint arg -> [arg]
            | TACGoto _
            | TACLabel _ -> []
            )
        |> List.filter isRealOperand
        |> List.distinct

    let assignSlot(slotSize : int, operands : Operand list) =
        operands
            |> List.indexed
            |> List.map (fun (i, op) -> op, -(i+1) * slotSize)
            |> Map.ofList
    
    let allocateStackSlots operands =
        let slotSize = 4
        let map = assignSlot(slotSize, operands)
        let frameSize = Map.count map * slotSize
        map, frameSize
        
    let buildFrame tac =
        let operands = collectOperands tac
        let defs = collectDefs tac
        let uses = collectUses tac
        printfn "Defs: %A" defs
        printfn "Uses: %A" uses
        let map, frameSize = allocateStackSlots operands
        map, frameSize
    
    let slotOf (map : Map<Operand, int>, operand : Operand) =
        match operand with
        | OpImmInt _ | OpImmFloat _ ->
            raise(GenerationException("Attemped to get immediate value from stack"))
        | _ -> map.[operand]
        
    let loadStackValue(map : Map<Operand, int>, register: int, operand : Operand) =
        match operand with
        | OpImmInt i ->
            sprintf $"li t{register}, {i}"
        | _ ->
            let offset = slotOf(map, operand)
            sprintf $"li t{register}, {offset}(s0)"
    
    let storeStackValue(map : Map<Operand, int>, register: int, operand : Operand) =
        let offset = slotOf(map, operand)
        sprintf $"sw {register}, {offset}(s0)"
        
    let riscvPreamble frameSize =
        sprintf $"addi sp, sp, -{frameSize}"

    let tacToRisc (tac : TAC ) (map : Map<Operand, int>) =
        match tac with
        | TACBinary (t, x, op, y) ->
            match t, x, y with
            | OpTemp t, OpVar x, OpImmInt y ->
                                                let offset = slotOf(map, OpVar x)
                                                $"addi t{t}, {offset}(sp), {y},"
            | OpTemp t, OpTemp x, OpImmInt y -> $"addi t{t}, t{x}, {y},"
            | OpTemp t, OpVar x, OpVar y ->
                                            let xReg = 0
                                            let yReg = 0
                                            let xOffset = slotOf(map, OpVar x)
                                            let yOffset = slotOf(map, OpVar y)
                                            $"lw t{xReg}, {xOffset}(sp) \nlw t{yReg}, {yOffset}(sp) \nadd t{t}, t{xReg}, t{yReg}"
            | OpTemp t, OpTemp x, OpTemp y -> $"add t{t}, t{x}, t{y}"
            | _ -> raise(GenerationException("Unknown add format"))
        | TACAssign (x, y) ->
            match x, y with
            | OpVar x, OpVar y ->
                                    let t = 0
                                    let xOffset = 0
                                    let yOffset = 0
                                    $"lw t{t}, {yOffset}($sp) \nsw t{t}, {xOffset}(sp)"
            | OpVar x, OpTemp y ->
                                    let offset = slotOf(map, OpVar x)
                                    $"sw t{y}, {offset}(sp)"
            | OpTemp x, OpImmInt y ->
                                    $"li t{x}, {y}"
            | OpVar x, OpImmInt y ->
                                    let reg = 0
                                    let stackOffset = slotOf(map, OpVar x)
                                    $"li t{reg}, {y} \nsw t{reg}, {stackOffset}(sp)"
            | _ -> raise(GenerationException("Unknown assign format"))
        | _ -> ""
        
    let mutable tempCounter = 0
    let mutable labelCounter = 0
    let mutable typeMap : Map<string, string> = Map.empty 
        
    let setType (operand: Operand) (typeName: string) =
        match operand with
        | OpVar name -> typeMap <- typeMap.Add(name, typeName)
        | OpTemp t -> typeMap <- typeMap.Add($"t{t}", typeName)
        | _ -> ()

    let getType (operand: Operand) : string =
        match operand with
        | OpImmInt _ -> "int"
        | OpImmFloat _ -> "double"
        | OpVar name -> 
            match typeMap.TryFind(name) with
            | Some t -> t
            | None -> "int"  // Default to int
        | OpTemp t -> 
            match typeMap.TryFind($"t{t}") with
            | Some ty -> ty
            | None -> "int"  // Default to int

    let inferBinaryType (left: Operand) (right: Operand) : string =
        let leftType = getType left
        let rightType = getType right
        if leftType = "double" || rightType = "double" then "double"
        else "int"
    let assignTemp() =
        tempCounter <- tempCounter + 1
        tempCounter
    
    let newLabel(prefix: string) =
        labelCounter <- labelCounter + 1
        $"{prefix}_{labelCounter}"    
    
    let tacToString tac =
        match tac with
        | TACAssign (x, y) -> $"{operandToString x} = {operandToString y};"
        | TACBinary (t, x, op, y) -> $"{operandToString t} = {operandToString x} {op} {operandToString y};"
        | TACUnary (t, op, x) -> $"{operandToString t} = {op} {operandToString x};"
        | TACCall (t, funcName, args) -> $"{operandToString t} = {funcName}({(operandListToString args)});" // We need to differentiate void functions
                                                                            // and assign return value if not void or we assume there are no void functions?
        | TACEquiv (t, x, op, y) -> $"{operandToString t} = {operandToString x} {op} {operandToString y};"
        | TACIf (cond, label) -> $"if ({operandToString cond}) goto {label};"
        | TACGoto label -> $"goto {label};"
        | TACLabel label -> $"{label}:"
        | TACPrint arg -> 
            let argType = getType arg
            let format = if argType = "double" then "%f" else "%d"
            $"printf(\"{format}\\n\", {operandToString arg});"
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
            | '{'::tail -> Lcurl:: scan tail
            | '}'::tail -> Rcurl:: scan tail
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
    // <F>         ::= <IfStmt> | <ForLoop> | <WhileLoop> | <NR> | <FCall> 
    // <IfStmt>    ::= "if" "(" <Comp> ")" "then" "{" <Prog> "}" ("else" "{" <Prog> "}")?
    // <ForLoop>   ::= "for" "(" Id "=" <Comp> "to" <Comp> ")" "do" "{" <Prog> "}"
    // <WhileLoop> ::= "while" "(" <Comp> ")" "do" "{" <Prog> "}"
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
        | WhileLoop(condExpr, bodyStatements) ->
            let rec loop lastResult =
                let condValue = astEvaluate condExpr
                let isTrue =
                    match condValue with
                    | IntVal n -> n <> 0
                    | FloatVal f -> f <> 0.0
                if isTrue then
                    let newResult =
                        bodyStatements
                        |> List.fold ( fun _ stmt -> astEvaluate stmt) (IntVal 0)
                    loop newResult
                else
                    lastResult
            loop (IntVal 0)

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
            let rec parseBlock tokens = // We need this helper function so that the parser can extract the code block
                let rec loop acc t =
                    match t with
                    | Rcurl :: rest -> rest, Prog(List.rev acc)
                    | Semi :: rest -> loop acc rest
                    | [] -> raise (ParseException "Expected ')' to close block")
                    | _ ->
                        let (rest, stmt) = S t
                        loop (stmt :: acc) rest
                loop [] tokens
            
            match tList with
            | Id "if" :: Lpar :: tail ->
                let (restCond, condExpr) = Comp tail
                match restCond with
                | Rpar :: Id "then" :: Lcurl :: thenTail ->
                    let (restThen, thenExpr) = parseBlock thenTail
                    match restThen with
                    | Id "else" :: Lcurl :: elseTail ->
                        let (restElse, elseExpr) = parseBlock elseTail
                        restElse, IfExpr(condExpr, thenExpr, Some elseExpr)
                    | _ -> restThen, IfExpr(condExpr, thenExpr, None)
                | _ -> raise(ParseException "Unknown conditional form")
                
            | Id "for" :: Lpar :: Id varName :: Eq :: tail ->
                let (rest1, startExpr) = Comp tail
                match rest1 with
                | Id "to" :: tail2 ->
                    let (rest2, endExpr) = Comp tail2
                    match rest2 with
                    | Rpar :: Id "do" :: Lcurl :: bodyTail ->
                        match parseBlock bodyTail with
                        | rest3, Prog bodyList ->
                            (rest3, ForLoop(varName, startExpr, endExpr, bodyList))
                        | _ -> raise (ParseException "Block did not return a Prog node")
                    | _ -> raise (ParseException "Expected ') do {' in for loop")
                | _ -> raise (ParseException "Expected 'to' in for loop")

            | Id "while" :: Lpar :: tail ->
                let (restCond, condExpr) = Comp tail
                match restCond with
                | Rpar :: Id "do" :: Lcurl :: bodyTail ->
                    match parseBlock bodyTail with
                    | restBody, Prog bodyList ->
                        (restBody, WhileLoop(condExpr, bodyList))
                    | _ -> raise (ParseException "Block did not return a Prog node")
                | _ -> raise (ParseException "Expected ') do {' in while loop")

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
            | _ ->
                printf $"NR: {tList}"
                raise (ParseException "Unexpected token in factor")
        
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
            
        match tList with
        | [] -> raise (ParseException "Empty input")
        | _ ->
            let (rest, expr) = Program tList
            (rest, expr)




    
    let operandToCDecl = function
    | OpTemp t -> $"t{t}"
    | _ -> failwith "Expected Temp in declareTemps"
          
    let declareTemps tac =
        tempCounter <- 0
        let allOperands = collectOperands tac
        let temps = 
            allOperands 
            |> List.choose (function 
                | OpTemp t -> Some $"t{t}"
                | _ -> None)
            |> List.distinct
            |> List.sort
        let vars = 
            allOperands 
            |> List.choose (function 
                | OpVar v -> Some v 
                | _ -> None)
            |> List.distinct
            |> List.sort

        let allNames = temps @ vars

        let intVars = 
            allNames 
            |> List.filter (fun name -> 
                match typeMap.TryFind(name) with
                | Some "int" -> true
                | Some "double" -> false
                | None -> true
            )
        
        let doubleVars = 
            allNames 
            |> List.filter (fun name -> 
                match typeMap.TryFind(name) with
                | Some "double" -> true
                | _ -> false
            )

        let declarations = []
        let declarations = 
            if intVars.IsEmpty then declarations
            else ("int " + String.concat ", " intVars + ";") :: declarations
        let declarations = 
            if doubleVars.IsEmpty then declarations
            else ("double " + String.concat ", " doubleVars + ";") :: declarations

        String.concat "\n" (List.rev declarations)
        
    let rec flattenIRtoTAC ir =
        match ir with
        | Int (IntVal value) ->
            [], OpImmInt value
        | Int (FloatVal value) ->
            [], OpImmFloat value 
        | Var name ->
            [], OpVar name
        | Assign(varName, expr) ->
            match expr with
            | Int (IntVal i) -> 
                let op = OpImmInt i
                setType (OpVar varName) "int"
                [TACAssign(OpVar varName, op)], OpVar varName
            | Int (FloatVal f) ->
                let op = OpImmFloat f
                setType (OpVar varName) "double" 
                [TACAssign(OpVar varName, op)], OpVar varName
            | _ ->
                let (code, t) = flattenIRtoTAC expr
                let exprType = getType t
                setType (OpVar varName) exprType
                code @ [TACAssign(OpVar varName, t)], OpVar varName
        | Unary(op, expr) ->
            let (code, t) = flattenIRtoTAC expr
            let temp = assignTemp()
            let resultType = getType t
            setType (OpTemp temp) resultType
            code @ [TACUnary(OpTemp temp, op, t)], OpTemp temp
        | Binary(x, op, y) ->
            let (codeL, l) = flattenIRtoTAC x
            let (codeR, r) = flattenIRtoTAC y
            let resultType = inferBinaryType l r
            let temp = assignTemp()
            setType (OpTemp temp) resultType
            codeL @ codeR @ [TACBinary(OpTemp temp, l, op, r)], OpTemp temp
        | Power(x, y) ->
            let (codeL, l) = flattenIRtoTAC x
            let (codeR, r) = flattenIRtoTAC y
            let temp = assignTemp()
            setType (OpTemp temp) "double"
            codeL @ codeR @ [TACBinary(OpTemp temp, l ,"^", r)], OpTemp temp
        | Eqiv(x, op, y) ->
            let (codeL, l) = flattenIRtoTAC x
            let (codeR, r) = flattenIRtoTAC y
            let temp = assignTemp()
            setType (OpTemp temp) "int"
            codeL @ codeR @ [TACEquiv(OpTemp temp, l, op, r)], OpTemp temp
        | FunCall(funcName, args) ->
            if funcName = "print" then
                let argTupleList = args |> List.map flattenIRtoTAC
                let argList = argTupleList |> List.collect fst
                let tempList = argTupleList |> List.map snd
                let printArg = tempList |> List.head
                argList @ [TACPrint printArg], OpImmInt 0
            else
                let argTupleList = args |> List.map flattenIRtoTAC
                let argList = argTupleList |> List.collect fst
                let tempList = argTupleList |> List.map snd
                let lastTemp = tempList |> List.last
                let temp = assignTemp()
                setType (OpTemp temp) "double"
                argList @ [TACCall(OpTemp temp, funcName, tempList)], OpTemp temp
        | IfExpr(condExpr, thenExpr, elseExpr) ->
            let (condCode, condTemp) = flattenIRtoTAC condExpr
            let thenLabel = newLabel "then"
            let elseLabel = newLabel "else"
            let endLabel  = newLabel "endif"
            let (thenCode, thenTemp) = flattenIRtoTAC thenExpr
            let elseCode, elseTemp =
                match elseExpr with
                | Some e -> flattenIRtoTAC e
                | None -> ([], OpImmInt 0)
            let resultTemp = assignTemp()
            let resultType = inferBinaryType thenTemp elseTemp
            setType (OpTemp resultTemp) resultType            
            let code =
                condCode @
                [ TACIf(condTemp, thenLabel)
                  TACGoto elseLabel
                  TACLabel thenLabel ] @
                thenCode @
                [ TACAssign(OpTemp resultTemp, thenTemp)
                  TACGoto endLabel
                  TACLabel elseLabel ] @
                elseCode @
                [ TACAssign(OpTemp resultTemp, elseTemp)
                  TACLabel endLabel ]

            code, OpTemp resultTemp
        | ForLoop(varName, startExpr, endExpr, bodyStatements) ->
            let (startCode, startTemp) = flattenIRtoTAC startExpr
            let (endCode, endTemp) = flattenIRtoTAC endExpr
            let loopStart = newLabel "for_loop"
            let loopEnd = newLabel "for_end"
            let bodyProg = Prog(bodyStatements)
            let (bodyCode, bodyTemp) = flattenIRtoTAC bodyProg
            let condTemp = assignTemp()
            let incTemp = assignTemp()
            let resultTemp = assignTemp()
            setType (OpVar varName) "int"
            setType (OpTemp condTemp) "int"
            setType (OpTemp incTemp) "int"
            let bodyType = getType bodyTemp
            setType (OpTemp resultTemp) bodyType

            let code =
                startCode @                              
                [TACAssign(OpVar varName, startTemp)] @        
                endCode @                                
                [TACLabel loopStart] @                   
                [TACEquiv(OpTemp condTemp, OpVar varName, ">", endTemp)] @  
                [TACIf(OpTemp condTemp, loopEnd)] @             
                bodyCode @                               
                [TACAssign(OpTemp resultTemp, bodyTemp)] @      
                [TACBinary(OpTemp incTemp, OpVar varName, "+", OpImmInt 1)] @ 
                [TACAssign(OpVar varName, OpTemp incTemp)] @          
                [TACGoto loopStart] @                    
                [TACLabel loopEnd]                       
            
            code, OpTemp resultTemp
            
        | WhileLoop(condExpr, bodyStatements) ->
            
            let loopStart = newLabel "while_loop"
            let loopEnd = newLabel "while_end"
            let (condCode, condTemp) = flattenIRtoTAC condExpr
            let bodyProg = Prog(bodyStatements)
            let (bodyCode, bodyTemp) = flattenIRtoTAC bodyProg
            let resultTemp = assignTemp()
            let notTemp = assignTemp()
            setType (OpTemp notTemp) "int"
            let bodyType = getType bodyTemp
            setType (OpTemp resultTemp) bodyType
            let code =
                [TACLabel loopStart] @                   
                condCode @                               
                [TACUnary(OpTemp notTemp, "!", condTemp)] @     
                [TACIf(OpTemp notTemp, loopEnd)] @              
                bodyCode @                               
                [TACAssign(OpTemp resultTemp, bodyTemp)] @      
                [TACGoto loopStart] @                    
                [TACLabel loopEnd]                       
            
            code, OpTemp resultTemp
        | Prog exprs ->
            let codeList = exprs |> List.map flattenIRtoTAC // Take each line and apply flatten
            let tac = codeList |> List.collect fst          // Take each code block and collect into one list
            let lastVar = codeList |> List.map snd |> List.last // Get last variable assigned which is the final result
            tac, lastVar
        | _ ->
            printf "%A\n" ir
            raise (ParseException("Unknown IR token during flattening"))

    let toJson(result: EvalResult) =
        match result with
        | Number n -> JsonConvert.SerializeObject({| ``type`` = "number"; value = n |})    
        | Plot(xs, ys) -> JsonConvert.SerializeObject({| ``type`` = "plot"; x = xs; y = ys |})
        
    let evalPlot (expr: string, xMin: float, xMax: float, stepSize: float) : string =
        let xs = ResizeArray<float>()
        let ys = ResizeArray<float>()

        for x in seq { xMin .. stepSize .. xMax } do
            let replacement = $"({ x.ToString(System.Globalization.CultureInfo.InvariantCulture) })"
            let substituted = expr.Replace("x", replacement)
            let lexed = lexer substituted
            let (_, ast) = parse lexed

            try
                let result = astEvaluate ast
                let y = toFloat result
                if not (Double.IsNaN y) && not (Double.IsInfinity y) then
                    xs.Add(x)
                    ys.Add(y)
            with
            | :? DivisionByZeroException ->
                xs.Add(x)
                ys.Add(Double.NaN)
            | _ -> ()

        let res = Plot(xs.ToArray(), ys.ToArray())
        toJson(res)

    let rec printTList (lst:list<terminal>) : list<string> = 
        match lst with
        head::tail -> Console.Write("{0} ",head.ToString())
                      printTList tail
                      
        | [] -> Console.Write("EOL\n")
                []
                
//    let printValue = function
//    | IntVal x -> string x
//    | FloatVal f -> string f
//    | ComplexVal (r, i) -> complexToString (r, i)

    let writeToFile (fileName : string, str : string, path : string) =
        let result = System.IO.Directory.CreateDirectory(path)
        let path = Path.Combine(path, fileName)
        let file = File.Create(path)
        
        let bytes = System.Text.Encoding.UTF8.GetBytes(str)
        do file.WriteAsync(ReadOnlyMemory bytes) |> ignore
        
        file.Close()
        //File.Delete(path)
    
    let tacRISCVString (taclist : TAC list) (map : Map<Operand, int>)=
        let header = riscvPreamble 
        let body =
            taclist
            |> List.map (fun tac -> tacToRisc tac map)
            |> String.concat "\n"
        body
        
    let tacCString tac =
        let tempDecs = declareTemps tac
        let body = tac
                    |> List.map tacToString
                    |> String.concat "\n"
        $"#include <math.h>\n#include <stdio.h>\nint main(){{\n{tempDecs}\n\n{body}\nreturn 0;\n}}"
            
            
    let evaluate(expr: string) : NumericValue =
        let tokens = lexer expr
        let (_, ast) = parse tokens
        printfn "AST: %A" ast
        let result = astEvaluate ast
        result  
    
    let riscvCompile(expr: string) : string =
        let tokens = lexer expr
        printfn "Tokens: %A" tokens
        let (_, ast) = parse tokens
        printfn "AST: %A" ast
        let (tac, last) = flattenIRtoTAC ast
        printfn "TAC: %A" tac
        let map, frameSize = buildFrame tac
        printfn "Map: %A" map
        printfn "Frame Size: %A" frameSize
        let header = riscvPreamble frameSize
        let body = tacRISCVString tac map
        let code = sprintf $"{header}\n{body}"
        code
        
    
    let cCompile(expr: string) : string =
        typeMap <- Map.empty 
        let tokens = lexer expr
        printfn "Tokens: %A" tokens
        let (_, ast) = parse tokens
        printfn "AST: %A" ast
        let (tac, last) = flattenIRtoTAC ast
        let str = tacCString tac
        str
    
    let gccCompile (workingDirectory : string) (src : string) (dest : string) =
        let flags = "-Wall -lm"
        
        let psi =
            ProcessStartInfo(
                FileName = "gcc",
                Arguments = $"{flags} {src} -o {dest}",
                WorkingDirectory = workingDirectory,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                CreateNoWindow = true
            )
        let proc = new Process()
        proc.StartInfo <- psi
        proc.Start() |> ignore
        
        let stdout = proc.StandardOutput.ReadToEnd()
        let stderr = proc.StandardError.ReadToEnd()
        
        proc.WaitForExit()
        
        let json = JsonConvert.SerializeObject({| ``type`` = "compile"; exit = proc.ExitCode; out = stdout; err = stderr |})
        
        json
        
            
    [<EntryPoint>]
    let main argv  =
        Console.WriteLine("Simple Interpreter")
        
        // Test For Loop
        let forTest = "x = sin(1); y = x; for(i = 1 to 5) do { x = x + i }"
        let forCompiled = cCompile(forTest)
        //writeToFile("for_test.c", forCompiled)
        printfn "%s" forCompiled
        
        // Test While Loop
        let whileTest = "x = 0; while(x < 5) do { x = x + 1 }"
        let whileCompiled = cCompile(whileTest)
        //writeToFile("while_test.c", whileCompiled)
        printfn "%s" whileCompiled
            
        
        // Test if
        let compilerInput = "x = 5; y = 6.3; z = x+y; print(z); if(x < 1) then { 2*2 }"
        let compiled = cCompile(compilerInput)
        let outputPath = Path.Combine(Path.GetDirectoryName(__SOURCE_DIRECTORY__), "out")
        writeToFile("if_test.c", compiled, outputPath)
        printfn "Compiled successfully!"
        
        // Test evaluator
        let evalInput = "5 + 3 * 2; print(2+2222)"
        let result = evaluate evalInput
        printfn "Result: %A" result
        printfn "Symbol Table: %A" symbTable
        
        // Test RISC-V Compiler
        let compilerInput = "x = 5; y = 6; z = x + y;"
        let compiled = cCompile(compilerInput)
        printfn "Result: %A" compiled
        //writeToFile("gui_test.c", compiled)
        
        0
