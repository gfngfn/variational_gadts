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


type TypeIdent =
  string


type ManualTypeVar =
  string


type ManualType =
  Range * ManualTypeMain


and ManualTypeMain =
  | MTypeVar  of ManualTypeVar
  | MDataType of TypeIdent * ManualType list
  | MFuncType of ManualType * ManualType


type BaseConstant =
  | UnitValue
  | BooleanValue of bool
  | IntegerValue of int


type Constructor =
  | Ctor of Range * string

  override this.ToString() =
    match this with
    | Ctor(r, s) -> sprintf "Ctor(%O, \"%s\")" r s


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
    | Constructor(Ctor(_, s), es) -> sprintf "Constructor(%s, %O)" s es


and ValueBinding =
  | NonRec of Ident * Ast
  | Rec    of Ident * Ast

  override this.ToString () =
    match this with
    | NonRec(i, e1) -> sprintf "NonRec(%O, %O)" i e1
    | Rec(i, e1)    -> sprintf "Rec(%O, %O)" i e1


type GeneralizedConstructorBranch =
  | GeneralizedConstructorBranch of Constructor * ManualTypeVar list * ManualType list * TypeIdent * ManualType list


type TypeBinding =
  Generalized of TypeIdent * int * GeneralizedConstructorBranch list


type Binding =
  | BindValue of ValueBinding
  | BindType  of TypeBinding


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
    | _                      -> invalidArg "obj" "not of type DataTypeId"


type ChoiceId private(n : int) =
  static let mutable current = 0

  new() =
    current <- current + 1
    new ChoiceId(current)

  member this.Number = n

  override this.ToString() =
    sprintf "C%d" n

  override this.GetHashCode() =
    this.Number.GetHashCode()

  override this.Equals(obj : obj) =
    match obj with
    | :? ChoiceId as other -> this.Number = other.Number
    | _                    -> invalidArg "obj" "not of type ChoiceId"

  interface IComparable<ChoiceId> with
    member this.CompareTo(other: ChoiceId) : int =
      this.Number - other.Number

  interface IComparable with
    member this.CompareTo(obj: obj) =
      match obj with
      | :? ChoiceId as other -> this.Number - other.Number
      | _                    -> invalidArg "obj" "not of type ChoiceId"


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


type VariationalType<'a> =
  Range * VariationalTypeMain<'a>


and VariationalTypeMain<'a> =
  | VTypeVar    of 'a
  | VDataType   of DataTypeId * VariationalType<'a> list
  | VFuncType   of VariationalType<'a> * VariationalType<'a>
  | VChoiceType of ChoiceId * VariationalType<'a> list


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
    Vars   : Map<string, PolyType>;
    Ctors  : Map<string, ConstructorDef>;
    TyVars : Map<ManualTypeVar, BoundId>;
    Types  : Map<TypeIdent, DataTypeId * int>;
  }

  static member empty =
    {
      Vars   = Map.empty;
      Ctors  = Map.empty;
      TyVars = Map.empty;
      Types  = Map.empty;
    }

  member this.FoldValue(f, init) =
    this.Vars |> Map.fold f init

  member this.FoldConstructor(f, init) =
    this.Ctors |> Map.fold f init

  member this.FoldType(f, init) =
    this.Types |> Map.fold f init

  member this.TryFindValue(x) =
    this.Vars.TryFind(x)

  member this.AddValue(x, pty) =
    { this with Vars = this.Vars.Add(x, pty) }

  member this.AddConstructor(ctor, ctordef) =
    { this with Ctors = this.Ctors.Add(ctor, ctordef) }

  member this.TryFindConstructor(ctor) =
    this.Ctors.TryFind(ctor)

  member this.AddTypeVariable(tyvar, bid) =
    { this with TyVars = this.TyVars.Add(tyvar, bid) }

  member this.TryFindTypeVariable(tyvar) =
    this.TyVars.TryFind(tyvar)

  member this.AddType(tyident, dtid, arity) =
    { this with Types = this.Types.Add(tyident, (dtid, arity)) }

  member this.TryFindType(tyident) =
    this.Types.TryFind(tyident)


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
