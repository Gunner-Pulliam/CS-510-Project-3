namespace MicroMLParser

type Expr =
    | Var of string
    | Int of int
    | Fun of string * Expr
    | App of Expr * Expr
    | Let of string * Expr * Expr

module Parser =
    open FParsec

    type UserState = unit
    type Parser<'t> = Parser<'t, UserState>

    let ws = spaces
    let str_ws s = pstring s .>> ws

    let identifier: Parser<string> =
        many1SatisfyL System.Char.IsLetter "identifier" .>> ws

    let number: Parser<Expr> =
        pint32 .>> ws |>> Int

    let varExpr: Parser<Expr> =
        identifier |>> Var

    let parens p = between (str_ws "(") (str_ws ")") p

    let rec expr (): Parser<Expr> =
        let simpleExpr =
            choice [
                attempt number
                attempt varExpr
                parens (expr ())
            ]

        // This safely folds left-associative applications
        let appExpr: Parser<Expr> =
            many1 simpleExpr
            |>> function
                | f :: args -> List.fold (fun acc arg -> App(acc, arg)) f args
                | [] -> failwith "Unexpected empty application"

        let funExpr =
            str_ws "fun" >>. identifier .>> str_ws "->" .>>. expr ()
            |>> Fun

        let letExpr =
            str_ws "let" >>. identifier .>> str_ws "=" .>>. expr () .>>. (str_ws "in" >>. expr ())
            |>> fun ((v, e1), e2) -> Let(v, e1, e2)

        choice [
            attempt letExpr
            attempt funExpr
            attempt appExpr
        ]



    let parseMicroML (code: string): Result<Expr, string> =
        match run (ws >>. expr () .>> eof) code with
        | Success(result, _, _) -> Result.Ok result
        | Failure(msg, _, _) -> Result.Error msg



module AstPrinter =
    let rec toJson (expr: Expr) : string =
        match expr with
        | Var v -> $"{{ \"type\": \"Var\", \"name\": \"{v}\" }}"
        | Int i -> $"{{ \"type\": \"Int\", \"value\": {i} }}"
        | Fun (arg, body) ->
            $"{{ \"type\": \"Fun\", \"arg\": \"{arg}\", \"body\": {toJson body} }}"
        | App (f, x) ->
            $"{{ \"type\": \"App\", \"func\": {toJson f}, \"arg\": {toJson x} }}"
        | Let (v, rhs, body) ->
            $"{{ \"type\": \"Let\", \"name\": \"{v}\", \"rhs\": {toJson rhs}, \"body\": {toJson body} }}"

module DummyParser =
    open Parser

    let parseToJson code =
        match parseMicroML code with
        | Ok ast -> AstPrinter.toJson ast
        | Error msg -> $"{{ \"error\": \"{msg}\" }}"