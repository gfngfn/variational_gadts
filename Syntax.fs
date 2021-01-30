module Syntax

open FParsec

type Range = (Position * Position) option

type Ident =
  | Ident of Range * string

type Ast = Range * AstMain

and AstMain =
  | Var    of Ident
  | Apply  of Ast * Ast
  | Lambda of Ident * Ast
