module Lib

open FParsec.CharParsers

// Define a function to construct a message to print
let from s =
  match run pfloat s with
  | Success(res, _, _) -> sprintf "success: %A" res
  | Failure(msg, _, _) -> sprintf "failure: %s" msg
