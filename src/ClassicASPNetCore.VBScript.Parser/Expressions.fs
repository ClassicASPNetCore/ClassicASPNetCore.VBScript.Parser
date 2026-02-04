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
    let private term = 
        choice [
            pliteral
            attempt pCall
            pvariable
            parens exprParser.ExpressionParser
        ]
    exprParser.TermParser <- term .>> inlineSpaces
    
    let opp = exprParser
    let addInfix str prec assoc op =
        opp.AddOperator(InfixOperator(str, inlineSpaces, prec, assoc, (fun l r -> BinaryOperation(l, op, r))))
    let addPrefix str prec op =
        opp.AddOperator(PrefixOperator(str, inlineSpaces, prec, true, (fun e -> UnaryOperation(op, e))))

    addInfix "^" 100 Associativity.Left Exponentiation
    addPrefix "-" 110 UnaryNegation
    addInfix "*" 90 Associativity.Left Multiplication
    addInfix "/" 90 Associativity.Left Division
    addInfix "\\" 80 Associativity.Left IntegerDivision
    addInfix "Mod" 70 Associativity.Left Modulus
    addInfix "mod" 70 Associativity.Left Modulus
    addInfix "+" 60 Associativity.Left Addition
    addInfix "-" 60 Associativity.Left Subtraction
    addInfix "&" 50 Associativity.Left StringConcatenation
    
    addInfix "=" 40 Associativity.Left Equality
    addInfix "<>" 40 Associativity.Left Inequality
    addInfix "<=" 40 Associativity.Left LessOrEqualThan
    addInfix ">=" 40 Associativity.Left GreaterOrEqualThan
    addInfix "<" 40 Associativity.Left LessThan
    addInfix ">" 40 Associativity.Left GreaterThan
    addInfix "Is" 40 Associativity.Left ObjectEquivalence
    addInfix "is" 40 Associativity.Left ObjectEquivalence

    addPrefix "Not" 35 LogicalNegation
    addPrefix "not" 35 LogicalNegation
    addInfix "And" 30 Associativity.Left LogicalConjunction
    addInfix "and" 30 Associativity.Left LogicalConjunction
    addInfix "Or" 20 Associativity.Left LogicalDisjunction
    addInfix "or" 20 Associativity.Left LogicalDisjunction
    addInfix "Xor" 15 Associativity.Left LogicalExclusion
    addInfix "xor" 15 Associativity.Left LogicalExclusion
    addInfix "Eqv" 10 Associativity.Left LogicalEquivalence
    addInfix "eqv" 10 Associativity.Left LogicalEquivalence
    addInfix "Imp" 5 Associativity.Left LogicalImplication
    addInfix "imp" 5 Associativity.Left LogicalImplication


    let pExpr = exprParser.ExpressionParser
    pArgExprListRef.Value <- sepBy pExpr (pword ",")