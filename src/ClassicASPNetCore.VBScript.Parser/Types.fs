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
    | UnaryOperation of (Operator * Expr)
    | BinaryOperation of (Expr * Operator * Expr)
    | Call of name: string * args: Expr list

type Argument = { Name: string; ByRef: bool }

type Statement =
    | Dim of variables:string list
    | Let of name:string * value:Expr
    | Set of name:string * value:Expr
    | If of condition:Expr * body:Block * elseIfs: (Expr * Block) list * Else:Block option
    | While of condition:Expr * body:Block
    | For of counter:string * startExpr:Expr * endExpr:Expr * step:Expr option * body:Block
    | ForEach of item:string * collection:Expr * body:Block
    | DoLoop of preCondition:(string * Expr) option * body:Block * postCondition:(string * Expr) option
    | SelectCase of test:Expr * cases:Case list * defaultCase:Block option
    | Exit of string
    | CallStmt of callExpr: Expr
and Block = Statement list
and Case = {Expressions: Expr list; Body: Block}

type Procedure =
    | Sub of name: string * args: Argument list * body: Block
    | Function of name: string * args: Argument list * body: Block

type TopLevelItem =
    | Procedure of Procedure
    | Statement of Statement