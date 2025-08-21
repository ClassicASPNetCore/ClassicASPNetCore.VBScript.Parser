namespace ClassicASPNetCore.VBScript.Parser
open FParsec

module ParserCore =
    let private comment: Parser<unit, Unit> = pstring "'" >>. skipRestOfLine true
    let cspaces = skipMany (choice [ spaces1; comment ])

    let pword s = pstringCI s .>> cspaces
    let parens p = between (pword "(") (pword ")") p

    let pIdentifier = many1Satisfy2 System.Char.IsLetter System.Char.IsLetterOrDigit .>> cspaces

    let pbool = pword "True" <|> pword "False" .>> cspaces |>> fun s -> s = "True" |> Boolean
    let pint = pint32 |>> int |>> Integer .>> cspaces
    let pstringliteral = between (pchar '"') (pchar '"') (manyChars (noneOf "\"")) |>> string |>> String .>> cspaces
    let pdate = between (pchar '#') (pchar '#') (manyChars (noneOf "#")) |>> string |>> Date .>> cspaces

    let pvalue = choice [ pint; pstringliteral; pbool; pdate ]
    let pliteral = pvalue |>> Literal