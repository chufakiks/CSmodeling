module Calculator
open Io.Calculator

open AST
open System

let rec evaluate (expr: expr) : Result<int, string> =
    match expr with
        |Num(x) -> Ok x
        |TimesExpr(a,b) -> 
            match evaluate a, evaluate b with
            | Ok(a), Ok(b) -> Ok(a*b)
            | Error e, _ | _, Error e -> Error e
        |DivExpr (a,b) -> 
            match evaluate a, evaluate b with
            | Ok(a), Ok(b) ->  if b <> 0 then Ok (a/b) else failwith "Undefined - Divided with 0"
            | Error e, _ | _, Error e -> Error e
        |PlusExpr (a,b) ->  
            match evaluate a, evaluate b with
            | Ok(a), Ok(b) -> Ok(a+b)
            | Error e, _ | _, Error e -> Error e
        |MinusExpr (a,b) ->
            match evaluate a, evaluate b with
            | Ok(a), Ok(b) -> Ok(a-b)
            | Error e, _ | _, Error e -> Error e                    
        |PowExpr (a,b) ->
            match evaluate a, evaluate b with
            | Ok(a), Ok(b) -> if b >= 0 then Ok(pown a b) else failwith "Undefined - Negative power"
            | Error e, _ | _, Error e -> Error e
        |UMinusExpr a ->
            match evaluate a with
            | Ok a  -> Ok (-a) 
            | Error e -> Error e


let analysis (input: Input) : Output =
    match Parser.parse Grammar.start_expression input.expression with
    | Ok ast ->
        Console.Error.WriteLine("> {0}", ast)
        match evaluate ast with
        | Ok result -> { result = result.ToString(); error = "" }
        | Error e -> { result = ""; error = String.Format("Evaluation error: {0}", e) }
    | Error e -> { result = ""; error = String.Format("Parse error: {0}", e) }

