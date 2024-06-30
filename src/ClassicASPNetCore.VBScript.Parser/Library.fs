namespace ClassicASPNetCore.VBScript.Parser
open FParsec

type Value =
    | Empty
    | Null
    | Boolean of bool
    | Byte of byte
    | Integer of int
    | Currency of decimal
    | Long of int64
    | Single of single
    | Double of double
    | Date of string
    | String of string
    | Object of obj
    | Error of int

type Operator =
    | Exponentiation
    | UnaryNegation
    | Multiplication
    | Division
    | IntegerDivision
    | Modulus
    | Addition
    | Subtraction
    | StringConcatenation
    | Equality
    | Inequality
    | LessThan
    | GreaterThan
    | LessOrEqualThan
    | GreaterOrEqualThan
    | ObjectEquivalence
    | LogicalNegation
    | LogicalConjunction
    | LogicalDisjunction
    | LogicalExclusion
    | LogicalEquivalence
    | LogicalImplication

type Expr =
    | Literal of Value
    | Variable of name:string
    | Operation of (Expr * Operator * Expr)

type Statement =
    | Set of name:string * value:Expr
    | If of condition:Expr * body:Block * Else:Block option
    | While of condition:Expr * body:Block
and Block = Statement list


module VBScriptParser =

    let private pword s = pstring s .>> spaces

    let private parens p = between (pword "(") (pword ")") p

    let private pbool =
        pword "True" <|> pword "False"
        |>> function
            | "True" -> Boolean (true)
            | "False" -> Boolean (false)

    let private pint = pint32 |>> int |>> Integer

    let private pstringliteral =
        pchar '\"' >>. manyCharsTill anyChar (pchar '\"')
        |>> string |>> String
        .>> spaces

    let pvalue =
        choice [
            pint
            pstringliteral
            pbool
        ]
    
    let pliteral = pvalue |>> Literal

    let pvariable =
        many1Satisfy2 (System.Char.IsLetter) (System.Char.IsLetterOrDigit)
        |>> Variable
        .>> spaces
    
    let intOperatorParser = OperatorPrecedenceParser<Expr, Unit, Unit>()

    let intExpr = intOperatorParser.ExpressionParser

    let intTerm = choice [
        pint .>> spaces |>> Literal <|> pvariable
        parens intExpr
    ]
    intOperatorParser.TermParser <- intTerm

    let createOperation op x y = Operation (x, op, y)

    let private pblocks = choice [
        pBlock
        pInstruction
    ]

    let parseString text =
        match runParserOnString (many pblocks) () "" text with
        | Success (result, _, _) -> List.toSeq result
        | Failure (error, _, _) -> raise (System.Exception(error))
