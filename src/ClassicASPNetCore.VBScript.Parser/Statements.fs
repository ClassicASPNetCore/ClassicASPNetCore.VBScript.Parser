// Statements.fs
namespace ClassicASPNetCore.VBScript.Parser
open FParsec
open ParserCore
open Expressions

module Statements =
    let pStatement, pStatementRef = createParserForwardedToRef<Statement, unit>()
    let pBlock, pBlockRef = createParserForwardedToRef<Block, unit>()
    pBlockRef.Value <- sepEndBy pStatement stmtSep

    let pDim =
        pword "Dim" >>. sepBy1 pIdentifier (inlineSpaces >>. pword ",")
        |>> Dim
    let pLet =
        pipe2 (pIdentifier .>> inlineSpaces) (pword "=" >>. pExpr) (fun name expr ->
            Let (name, expr)
        )
    
    let pSet =
        pipe2 (pword "Set" >>. pIdentifier .>> inlineSpaces) (pword "=" >>. pExpr) (fun name expr ->
            Set (name, expr)
        )
    
    let pIf =
        let pElseIf =
            pipe2 
                (pword "ElseIf" >>. pExpr .>> inlineSpaces .>> pword "Then" .>> cspaces) (pBlock .>> cspaces) (fun cond body -> (cond, body))
        pipe5
            (pword "If" >>. inlineSpaces >>. pExpr .>> inlineSpaces .>> pword "Then" .>> cspaces)
            pBlock
            (many pElseIf)
            (opt (pword "Else" >>. cspaces >>. pBlock))
            (pword "End" >>. pword "If" >>% ())
            (fun cond thenBlock elseIfs elseBlock _ ->
                If (cond, thenBlock, elseIfs, elseBlock)
            )

    let pWhile =
        pword "While" >>. inlineSpaces >>. pExpr .>> cspaces .>>.
        pBlock .>>
        (pword "Wend")
        |>> fun (cond, body) -> While (cond, body)

    let pFor =
        pipe5
            (pword "For" >>. inlineSpaces >>. pIdentifier .>> inlineSpaces)
            (pword "=" >>. inlineSpaces >>. pExpr .>> inlineSpaces)
            (pword "To" >>. inlineSpaces >>. pExpr .>> inlineSpaces)
            (opt (pword "Step" >>. inlineSpaces >>. pExpr .>> inlineSpaces))
            (cspaces >>. pBlock .>> cspaces .>> (pword "Next"))
            (fun counter start endExpr step body ->
                For (counter, start, endExpr, step, body)
            )
    
    let pForEach =
        pipe3
            (pword "For" >>. pword "Each" >>. inlineSpaces >>. pIdentifier .>> inlineSpaces)
            (pword "In" >>. inlineSpaces >>. pExpr .>> cspaces)
            (pBlock .>> cspaces .>> pword "Next")
            (fun item collection body ->
                ForEach (item, collection, body)
            )
    
    let pDoLoop =
        let pLoopCondition =
            pipe2
                (pword "While" <|> pword "Until" .>> inlineSpaces)
                (pExpr .>> inlineSpaces)
                (fun keyword expr -> (keyword, expr))
        pipe4
            (pword "Do" >>. opt pLoopCondition .>> cspaces)
            (pBlock .>> cspaces)
            (pword "Loop" .>> inlineSpaces)
            (opt pLoopCondition)
            (fun preCond body _ postCond ->
                DoLoop (preCond, body, postCond)
            )

    let pSelectCase =
        let pCase =
            notFollowedBy (pword "Case Else") >>. pword "Case" >>.
            pipe2
                (sepBy1 pExpr (pword ",") .>> cspaces)
                (pBlock .>> cspaces)
                (fun expr body -> { Expressions = expr; Body = body })
        
        let pDefaultCase =
            pword "Case Else" >>. cspaces >>. pBlock .>> cspaces
        
        pipe4
            (pword "Select Case" >>. pExpr .>> cspaces)
            (many pCase)
            (opt pDefaultCase)
            (pword "End" >>. pword "Select")
            (fun test cases defaultCase _ ->
                SelectCase (test, cases, defaultCase)
            )
    
    let pArgument =
        pipe2
            (opt (pword "ByVal" <|> pword "ByRef"))
            pIdentifier
            (fun modifier name ->
                let isByRef = defaultArg modifier "ByRef" = "ByRef"
                { Name = name; ByRef = isByRef })

    let pArgumentDefList = parens (sepBy pArgument (pword ","))

    let pSub =
        pipe3
            (pword "Sub" >>. pIdentifier)
            pArgumentDefList
            (cspaces >>. pBlock .>> pword "End" .>> pword "Sub")
            (fun name args body -> Sub (name, args, body))
    
    let pExit =
        pword "Exit" >>. (pword "Sub" <|> pword "Function" <|> pword "For" <|> pword "Do")
        |>> Exit
    
    let pCallStmt =
        pword "Call" >>. pExpr |>> CallStmt

    // Parser para "Function ... End Function"
    let pFunction =
        pipe3
            (pword "Function" >>. pIdentifier)
            pArgumentDefList
            (cspaces >>. pBlock .>> pword "End" .>> pword "Function")
            (fun name args body -> Function (name, args, body))

    pStatementRef.Value <-
        choice [
            pIf
            pWhile
            attempt pForEach
            pFor
            pDoLoop
            pSelectCase
            pSet
            pDim
            pExit
            pCallStmt
            attempt pLet
        ] .>> inlineSpaces

    let pTopLevelItem =
        choice [
            pSub |>> Procedure
            pFunction |>> Procedure
            pStatement |>> Statement
        ]

    let pScript = sepEndBy pTopLevelItem stmtSep

    let parseString text =
        let fullParser = cspaces >>. pScript .>> eof
        match runParserOnString fullParser () "" text with
        | Success (result, _, _) -> List.toSeq result
        | Failure (error, _, _) -> raise (System.Exception(error))