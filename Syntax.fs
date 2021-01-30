module Syntax

open FParsec

type Range =
  | DummyRange
  | ValidRange of Position * Position
  override this.ToString () =
    match this with
    | DummyRange             -> "DUMMY"
    | ValidRange(posL, posR) -> sprintf "%d:%d-%d:%d" posL.Line posL.Column posR.Line posR.Column

type Ident =
  | Ident of Range * string
  override this.ToString () =
    match this with
    | Ident(r, x) -> sprintf "Ident(%O, \"%s\")" r x

type Ast = Range * AstMain

and AstMain =
  | Var    of Ident
  | Apply  of Ast * Ast
  | Lambda of Ident * Ast
  override this.ToString () =
    match this with
    | Var(Ident(_, x)) -> sprintf "Var(\"%s\")" x
    | Apply(e1, e2)    -> sprintf "Apply(%O, %O)" e1 e2
    | Lambda(ident, e) -> sprintf "Lambda(%O, %O)" ident e
