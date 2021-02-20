module Parser

open FSharp.Core
open FParsec
open FParsec.Primitives
open FParsec.CharParsers

open Syntax


type ParseError = string


let makeRange ((r1, _) : Ast) ((r2, _) : Ast) : Range =
  match (r1, r2) with
  | (ValidRange(posL, _), ValidRange(_, posR)) -> ValidRange(posL, posR)
  | _                                          -> DummyRange


let withRange (p : Parser<'r, 'u>) : Parser<Range * 'r, 'u> =
  pipe3 getPosition p getPosition (fun posL v posR -> (ValidRange(posL, posR), v))


let identifierParser s =
  let pMain =
    let isIdentifierFirstChar c = isLetter c
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" >>= begin function
    | "fun"
    | "let" | "in" | "rec"
    | "if" | "then" | "else"
    | "true" | "false" ->
        fail "reserved word"

    | x ->
        preturn x
    end
  let p = withRange pMain |>> (fun (r, x) -> Ident(r, x))
  (attempt p) s


let integerParser s =
  let p = withRange (many1Satisfy isDigit |>> int)
  p s


let rec absLevelParser s =
  (letParser <|> ifParser <|> abstractionParser <|> appLevelParser) s


and letParser s =
  let p1 : Parser<bool * Ident, 'u> =
    let pRec : Parser<bool, 'u> =
       opt (pstring "rec" .>> spaces) |>> function
       | None    -> false
       | Some(_) -> true
    pstring "let" .>> spaces >>. pRec .>>. identifierParser .>> spaces .>> pstring "=" .>> spaces
  let p2f ((isRec, ident) : bool * Ident) : Parser<AstMain, 'u> =
    let p21 = absLevelParser .>> pstring "in" .>> spaces
    let p22 = absLevelParser
    pipe2 p21 p22 (fun e1 e2 ->
      let valbind =
        if isRec then
          Rec(ident, e1)
        else
          NonRec(ident, e1)
      LocalBinding(valbind, e2))
  let p = withRange (p1 >>= p2f)
  p s


and ifParser s =
  let p0 = pstring "if" .>> spaces >>. absLevelParser .>> spaces
  let p1 = pstring "then" .>> spaces >>. absLevelParser .>> spaces
  let p2 = pstring "else" .>> spaces >>. absLevelParser .>> spaces
  let p = withRange (pipe3 p0 p1 p2 (fun e0 e1 e2 -> IfThenElse(e0, e1, e2)))
  p s


and abstractionParser s =
  let p1 : Parser<Ident, 'u> =
    pstring "fun" .>> spaces >>. identifierParser .>> spaces .>> pstring "->" .>> spaces
  let p2f (ident : Ident) : Parser<AstMain, 'u> =
    absLevelParser >>= fun e -> preturn (Lambda(ident, e))
  let p = withRange (p1 >>= p2f)
  p s


and appLevelParser s =
  let p =
    many1 bottomLevelParser >>= function
    | []      -> failwith "bug"
    | e :: es -> preturn (List.fold (fun ef ex -> (makeRange ef ex, Apply(ef, ex))) e es)
  p s


and bottomLevelParser s =
  let pIdent = identifierParser >>= fun ident -> match ident with Ident(r, _) -> preturn (r, Var(ident))
  let pNil = withRange (pstring "[]" |>> fun _ -> Constructor("[]", []))
  let pTrue = withRange (pstring "true" |>> fun _ -> BaseConstant(BooleanValue(true)))
  let pFalse = withRange (pstring "false" |>> fun _ -> BaseConstant(BooleanValue(false)))
  let pUnitConst = withRange (pstring "()" |>> fun _ -> BaseConstant(UnitValue))
  let pIntConst = integerParser |>> fun (r, n) -> (r, BaseConstant(IntegerValue(n)))
  let pParen = between (pstring "(" .>> spaces) (pstring ")") absLevelParser
  let p = (pIdent <|> pNil <|> pTrue <|> pFalse <|> pUnitConst <|> pIntConst <|> pParen) .>> spaces
  p s


let parse (s : string) : Result<Ast, ParseError> =
  match run (spaces >>. absLevelParser) s with
  | Success(e, _, _)   -> Result.Ok(e)
  | Failure(msg, _, _) -> Result.Error(msg)
