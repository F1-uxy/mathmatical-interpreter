// Simple Interpreter in F#
// Author: R.J. Lapeer 
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

namespace MathInterpreter

module interpreter = 

    open System
    open MathInterpreter.Exceptions
    type terminal = 
        Add | Sub | Mul | Div | Mod | Pow | Lpar | Rpar | Num of int

    let str2lst s = [for c in s -> c]
    let isblank c = System.Char.IsWhiteSpace c
    let isdigit c = System.Char.IsDigit c
    
    let intVal (c:char) = (int)((int)c - (int)'0')

    let rec scInt(iStr, iVal) = 
        match iStr with
        c :: tail when isdigit c -> scInt(tail, 10*iVal+(intVal c))
        | _ -> (iStr, iVal)

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
            | c :: tail when isblank c -> scan tail
            | c :: tail when isdigit c -> let (iStr, iVal) = scInt(tail, intVal c) 
                                          Num iVal :: scan iStr
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
    // <P>        ::= <NR> <Popt>
    // <Popt>     ::= "^" <P> | <empty> 
    // <NR>       ::= "+" <NR> | "-" <NR> | "Num" <value> | "(" <E> ")"

    let parser tList = 
        let rec E tList = (T >> Eopt) tList         // >> is forward function composition operator: let inline (>>) f g x = g(f x)
        and Eopt tList = 
            match tList with
            | Add :: tail -> (T >> Eopt) tail
            | Sub :: tail -> (T >> Eopt) tail
            | _ -> tList
        and T tList = (NR >> Topt) tList
        and Topt tList =
            match tList with
            | Mul :: tail -> (P >> Topt) tail
            | Div :: tail -> (P >> Topt) tail
            | Mod :: tail -> (P >> Topt) tail
            | _ -> tList
        and P tList = (NR >> Popt) tList
        and Popt tList =
            match tList with
            | Pow :: tail -> P tail
            | _ -> tList
        and NR tList =
            match tList with
            | Add :: tail -> NR tail
            | Sub :: tail -> NR tail
            | Num value :: tail -> tail
            | Lpar :: tail -> match E tail with 
                              | Rpar :: tail -> tail
                              | _ -> raise (ParseException("Missing closing parenthesis"))
            | _ -> raise (ParseException("Invalid NR token"))
        E tList

    let parseNeval tList =
        let pown baseVal exp = int (System.Math.Pow(float baseVal, float exp))
        
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
            | _ -> raise (ParseException("Invalid NR token"))
        E tList

    let rec printTList (lst:list<terminal>) : list<string> = 
        match lst with
        head::tail -> Console.Write("{0} ",head.ToString())
                      printTList tail
                      
        | [] -> Console.Write("EOL\n")
                []

    let evaluate(expr: string) : int =
        let tokens = lexer expr
        let (_, result) = parseNeval tokens
        result

    [<EntryPoint>]
    let main argv  =
        Console.WriteLine("Simple Interpreter")
        let input:string = getInputString()
        let oList = lexer input
        let sList = printTList oList;
        let rList = parser oList
        let pList = printTList (rList)
        if not rList.IsEmpty then raise (ParseException("Trailing character in parser output"))
        let Out = parseNeval oList
        Console.WriteLine("Result = {0}", snd Out)
        0

