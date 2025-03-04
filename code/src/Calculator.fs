module Calculator
open Io.Calculator

open AST
open System

let rec evaluate (expr: expr) : Result<int, string> =
    match expr with
        |Num(x) -> Ok x
        |TimesExpr(Num(a), Num(b)) -> Ok(a*b)
        |DivExpr (Num(a), Num(b)) -> if b <> 0 then Ok (a/b) else failwith "Undefined - Divided with 0"
        |PlusExpr (Num(a), Num(b)) -> Ok (a + b)
        |MinusExpr (Num(a), Num(b)) -> Ok (a - b)
        |PowExpr (Num(a) , Num(b)) -> Ok (int(float a ** float b)) 
        |UMinusExpr (Num(x)) -> Ok -x
        |_ -> failwith "Unsupported operator"

let analysis (input: Input) : Output =
    match Parser.parse Grammar.start_expression input.expression with
    | Ok ast ->
        Console.Error.WriteLine("> {0}", ast)
        match evaluate ast with
        | Ok result -> { result = result.ToString(); error = "" }
        | Error e -> { result = ""; error = String.Format("Evaluation error: {0}", e) }
    | Error e -> { result = ""; error = String.Format("Parse error: {0}", e) }

