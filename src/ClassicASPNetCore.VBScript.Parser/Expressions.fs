namespace ClassicASPNetCore.VBScript.Parser
open FParsec
open ParserCore

module Expressions =
    let pvariable = pIdentifier |>> Variable
    
    // Referencia forward para la lista de argumentos en una llamada
    let private pArgExprList, pArgExprListRef = createParserForwardedToRef<Expr list, unit>()

    let private pCall = 
        pipe2 
            pIdentifier 
            (parens pArgExprList) 
            (fun name args -> Call (name, args))

    let exprParser = OperatorPrecedenceParser<Expr, unit, unit>()
    let private term = choice [
        pliteral
        pCall
        pvariable
        parens exprParser.ExpressionParser
    ]
    exprParser.TermParser <- term



    let pExpr = exprParser.ExpressionParser
    pArgExprListRef.Value <- sepBy pExpr (pword ",")