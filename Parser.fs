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
    | "fun" | "let" | "in" -> fail "reserved word"
    | x                    -> preturn x
    end
  let p = withRange pMain |>> (fun (r, x) -> Ident(r, x))
  (attempt p) s


let rec absLevelParser s =
  (letParser <|> abstractionParser <|> appLevelParser) s


and letParser s =
  let p1 : Parser<Ident, 'u> =
    pstring "let" .>> spaces >>. identifierParser .>> spaces .>> pstring "=" .>> spaces
  let p2f (ident : Ident) : Parser<AstMain, 'u> =
    let p21 = absLevelParser .>> pstring "in" .>> spaces
    let p22 = absLevelParser
    pipe2 p21 p22 (fun e1 e2 -> LetIn(ident, e1, e2))
  let p = withRange (p1 >>= p2f)
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
  let res = p s
  res


and bottomLevelParser s =
  let p1 = identifierParser >>= fun ident -> match ident with Ident(r, _) -> preturn (r, Var(ident))
  let p2 = between (pstring "(" .>> spaces) (pstring ")") absLevelParser
  let res = ((p1 <|> p2) .>> spaces) s
  res


let parse (s : string) : Result<Ast, ParseError> =
  match run absLevelParser s with
  | Success(e, _, _)   -> Result.Ok(e)
  | Failure(msg, _, _) -> Result.Error(msg)
