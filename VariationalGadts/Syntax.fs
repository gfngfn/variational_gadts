module Syntax

open System
open System.Collections.Generic
open FParsec


type Range =
  | DummyRange
  | ValidRange of Position * Position

  override this.ToString () =
    match this with
    | DummyRange ->
        "DUMMY"

    | ValidRange(posL, posR) ->
        let ln1 = posL.Line
        let ln2 = posR.Line
        if ln1 = ln2 then
          sprintf "%d:%d-%d" ln1 posL.Column posR.Column
        else
          sprintf "%d:%d-%d:%d" ln1 posL.Column ln2 posR.Column


type Ident =
  | Ident of Range * string

  override this.ToString () =
    match this with
    | Ident(r, x) -> sprintf "Ident(%O, \"%s\")" r x


type BaseConstant =
  | UnitValue
  | BooleanValue of bool
  | IntegerValue of int


type Constructor =
  string


type Ast =
  Range * AstMain


and AstMain =
  | Var          of Ident
  | Apply        of Ast * Ast
  | Lambda       of Ident * Ast
  | LocalBinding of ValueBinding * Ast
  | IfThenElse   of Ast * Ast * Ast
  | BaseConstant of BaseConstant
  | Constructor  of Constructor * Ast list

  override this.ToString () =
    match this with
    | Var(Ident(_, x))       -> sprintf "Var(\"%s\")" x
    | Apply(e1, e2)          -> sprintf "Apply(%O, %O)" e1 e2
    | Lambda(ident, e)       -> sprintf "Lambda(%O, %O)" ident e
    | LocalBinding(bind, e)  -> sprintf "LocalBinding(%O, %O)" bind e
    | IfThenElse(e0, e1, e2) -> sprintf "IfThenElse(%O, %O, %O)" e0 e1 e2
    | BaseConstant(bc)       -> sprintf "BaseConstant(%O)" bc
    | Constructor(ctor, es)  -> sprintf "Constructor(%s, %O)" ctor es


and ValueBinding =
  | NonRec of Ident * Ast
  | Rec    of Ident * Ast

  override this.ToString () =
    match this with
    | NonRec(i, e1) -> sprintf "NonRec(%O, %O)" i e1
    | Rec(i, e1)    -> sprintf "Rec(%O, %O)" i e1


type Binding =
  | BindValue of ValueBinding


type FreeId private(n : int) =

  static let mutable current = 0

  new () =
    current <- current + 1
    new FreeId(current)

  member this.Number = n

  override this.ToString() =
    sprintf "'%d" n


type BoundId private(n : int) =
  static let mutable current = 0

  new () =
    current <- current + 1
    new BoundId(current)

  member this.Number = n

  override this.ToString() =
    sprintf "#%d" n

  override this.GetHashCode() =
    this.Number.GetHashCode()

  override this.Equals(obj: obj) =
    match obj with
    | :? BoundId as other -> this.Number = other.Number
    | _                   -> invalidArg "obj" "not of type BoundId"

  interface IComparable<BoundId> with
    member this.CompareTo(other: BoundId) : int =
      this.Number - other.Number

  interface IComparable with
    member this.CompareTo(obj: obj) =
      match obj with
      | :? BoundId as other -> this.Number - other.Number
      | _                   -> invalidArg "obj" "not of type BoundId"


type DataTypeId private(n : int, s : string) =
  static let mutable current = 0

  new(name : string) =
    current <- current + 1
    new DataTypeId(current, name)

  member this.Number = n

  member this.Name = s

  override this.ToString() =
    sprintf "DT%d" n

  override this.GetHashCode() =
    this.Number.GetHashCode()

  override this.Equals(obj : obj) =
    match obj with
    | :? DataTypeId as other -> this.Number = other.Number
    | _                      -> invalidArg "obj" "not of type BoundId"


type Type<'a> =
  Range * TypeMain<'a>


and TypeMain<'a> =
  | TypeVar  of 'a
  | DataType of DataTypeId * Type<'a> list
  | FuncType of Type<'a> * Type<'a>

  override this.ToString () =
    match this with
    | TypeVar(tv)         -> sprintf "TypeVar(%O)" tv
    | DataType(dtid, tys) -> sprintf "BaseType(%O, %O)" dtid tys
    | FuncType(ty1, ty2)  -> sprintf "FuncType(%O, %O)" ty1 ty2


type MonoTypeVarUpdatable =
  | Free of FreeId
  | Link of MonoType

  override this.ToString () =
    match this with
    | Free(fid) -> sprintf "%O" fid
    | Link(ty)  -> sprintf "%O" ty


and MonoTypeVar =
  | Updatable of MonoTypeVarUpdatable ref

  override this.ToString () =
    match this with
    | Updatable(tvuref) ->
        sprintf "%O" !tvuref


and MonoType =
  Type<MonoTypeVar>


type PolyTypeVar =
  | Mono  of MonoTypeVar
  | Bound of BoundId


type PolyType =
  Type<PolyTypeVar>


type ConstructorDef =
  {
    BoundIds : BoundId list;
    MainType : PolyType;
    ArgTypes : PolyType list;
  }


type TypeEnv =
  private {
    Vars : Map<string, PolyType>;
    Ctors : Map<Constructor, ConstructorDef>
  }

  static member empty =
    {
      Vars = Map.empty;
      Ctors = Map.empty;
    }

  member this.FoldValue(f, init) =
    this.Vars |> Map.fold f init

  member this.TryFindValue(x) =
    this.Vars.TryFind(x)

  member this.AddValue(x, pty) =
    { this with Vars = this.Vars.Add(x, pty) }

  member this.AddConstructor(ctor, ctordef) =
    { this with Ctors = this.Ctors.Add(ctor, ctordef) }

  member this.TryFindConstructor(ctor) =
    this.Ctors.TryFind(ctor)


let unitTypeId = new DataTypeId("unit")
let boolTypeId = new DataTypeId("bool")
let intTypeId  = new DataTypeId("int")
let listTypeId = new DataTypeId("list")


let unitType rng =
  (rng, DataType(unitTypeId, []))


let boolType rng =
  (rng, DataType(boolTypeId, []))


let intType rng =
  (rng, DataType(intTypeId, []))


let listType rng ty =
  (rng, DataType(listTypeId, [ty]))


let (-->) ty1 ty2 =
  (DummyRange, FuncType(ty1, ty2))
