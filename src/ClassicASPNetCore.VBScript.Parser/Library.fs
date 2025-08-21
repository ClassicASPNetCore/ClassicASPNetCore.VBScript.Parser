namespace ClassicASPNetCore.VBScript.Parser
open FParsec
open ParserCore
open Statements


module VBScriptParser =
    let parseString text =
        let fullParser = cspaces >>. pScript .>> eof
        match runParserOnString fullParser () "" text with
        | Success (result, _, _) -> List.toSeq result
        | Failure (error, _, _) -> raise (System.Exception(error))
