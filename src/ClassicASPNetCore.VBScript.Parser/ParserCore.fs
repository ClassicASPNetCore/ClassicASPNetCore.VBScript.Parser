namespace ClassicASPNetCore.VBScript.Parser
open FParsec

module ParserCore =
    let private whitespace c = 
        c = ' ' || c = '\t'

    let private inlineWhitespace : Parser<unit, unit> =
        skipMany (satisfy whitespace)
    
    let private inlineWhitespace1 : Parser<unit, unit> =
        skipMany1 (satisfy whitespace)
    let private pLineContinuation = 
        pchar '_' >>. (inlineWhitespace .>>. newline)
        |>> ignore
        <?> "Expected whitespaces or new line only after line continuation"
    let private pComment: Parser<unit, Unit> = pstring "'" >>. skipRestOfLine true

    let inlineSpaces = skipMany (choice [
        pLineContinuation;
        pComment;
        inlineWhitespace1
    ])

    let stmtSep =
        inlineSpaces >>.
        (many1 (choice [
            skipNewline;
            skipChar ':'
        ])) .>>. inlineSpaces |>> ignore

    let pword s = pstringCI s .>> inlineSpaces
    let parens p = between (pword "(") (pword ")") p

    let pIdentifier: Parser<string, unit> = many1Satisfy2 System.Char.IsLetter System.Char.IsLetterOrDigit

    let pbool = pword "True" <|> pword "False" .>> inlineSpaces |>> fun s -> s = "True" |> Boolean
    let pint = pint32 |>> int |>> Integer
    let pstringliteral = between (pchar '"') (pchar '"') (manyChars (noneOf "\"")) |>> string |>> String
    let pdate = between (pchar '#') (pchar '#') (manyChars (noneOf "#")) |>> string |>> Date

    let pvalue = choice [ pint; pstringliteral; pbool; pdate ]
    let pliteral = pvalue |>> Literal