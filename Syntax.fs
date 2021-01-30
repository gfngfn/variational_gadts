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

type FreeId = int // temporary

type BoundId = int // temporary

type BaseType =
  | UnitType

type Type<'a> =
  Range * TypeMain<'a>

and TypeMain<'a> =
  | TypeVar  of 'a
  | BaseType of BaseType
  | FuncType of Type<'a> * Type<'a>

type MonoTypeVarUpdatable =
  | Free of FreeId
  | Link of MonoType

and MonoTypeVar =
  | Updatable of MonoTypeVarUpdatable ref

and MonoType = Type<MonoTypeVar>

type PolyTypeVar =
  | Mono  of MonoTypeVarUpdatable
  | Bound of BoundId

type PolyType = Type<PolyTypeVar>

let instantiate (pty : PolyType) : MonoType =
  failwith "TODO: instantiate"

let lift (ty : MonoType) : PolyType =
  failwith "TODO: lift"

let fresh () : FreeId =
  failwith "TODO: fresh"
