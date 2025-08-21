// Statements.fs
namespace ClassicASPNetCore.VBScript.Parser
open FParsec
open ParserCore
open Expressions

module Statements =
    let pStatement, pStatementRef = createParserForwardedToRef<Statement, unit>()
    let pBlock, pBlockRef = createParserForwardedToRef<Block, unit>()
    pBlockRef.Value <- many pStatement

    let pDim =
        pword "Dim" >>. sepBy1 pIdentifier (cspaces >>. pword ",")
        |>> Dim
    let pLet =
        pipe2 pIdentifier (pword "=" >>. pExpr) (fun name expr ->
            Let (name, expr)
        )
    
    let pSet =
        pipe2 (pword "Set" >>. pIdentifier) (pword "=" >>. pExpr) (fun name expr ->
            Set (name, expr)
        )
    
    let pIf =
        let pElseIf =
            pipe2 
                (pword "ElseIf" >>. pExpr .>> cspaces) (pBlock .>> cspaces) (fun cond body -> (cond, body))
        pipe5
            (pword "If" >>. pExpr .>> cspaces .>> pword "Then" .>> cspaces)
            pBlock
            (many pElseIf)
            (opt (pword "Else" >>. pBlock))
            (pword "End" >>. pword "If" >>% ())
            (fun cond thenBlock elseIfs elseBlock _ ->
                If (cond, thenBlock, elseIfs, elseBlock)
            )

    let pWhile =
        pword "While" >>. pExpr .>> cspaces .>>.
        pBlock .>>
        (pword "Wend")
        |>> fun (cond, body) -> While (cond, body)

    let pFor =
        pipe5
            (pword "For" >>. pIdentifier .>> cspaces)
            (pword "=" >>. pExpr .>> cspaces)
            (pword "To" >>. pExpr .>> cspaces)
            (opt (pword "Step" >>. pExpr .>> cspaces))
            (pBlock .>> (pword "Next"))
            (fun counter start endExpr step body ->
                For (counter, start, endExpr, step, body)
            )
    
    let pForEach =
        pipe3
            (pword "For Each" >>. cspaces >>. pIdentifier)
            (pword "In" >>. pExpr .>> cspaces)
            (pBlock .>> pword "Next")
            (fun item collection body ->
                ForEach (item, collection, body)
            )
    
    let pDoLoop =
        let pLoopCondition =
            pipe2
                (pword "While" <|> pword "Until" .>> cspaces)
                (pExpr .>> cspaces)
                (fun keyword expr -> (keyword, expr))
        pipe4
            (pword "Do" >>. opt pLoopCondition)
            (pBlock .>> cspaces)
            (pword "Loop" .>> cspaces)
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
            pword "Case Else" >>. pBlock .>> cspaces
        
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
            (pBlock .>> pword "End" .>> pword "Sub")
            (fun name args body -> Sub (name, args, body))

    // Parser para "Function ... End Function"
    let pFunction =
        pipe3
            (pword "Function" >>. pIdentifier)
            pArgumentDefList
            (pBlock .>> pword "End" .>> pword "Function")
            (fun name args body -> Function (name, args, body))

    pStatementRef.Value <-
        choice [
            pIf
            pWhile
            pForEach
            pFor
            pDoLoop
            pSelectCase
            pSet
            pDim
            attempt pLet
        ] .>> cspaces

    let pTopLevelItem =
        choice [
            pSub |>> Procedure
            pFunction |>> Procedure
            pStatement |>> Statement
        ]

    let pScript = many pTopLevelItem

    let parseString text =
        let fullParser = cspaces >>. pScript .>> eof
        match runParserOnString fullParser () "" text with
        | Success (result, _, _) -> List.toSeq result
        | Failure (error, _, _) -> raise (System.Exception(error))