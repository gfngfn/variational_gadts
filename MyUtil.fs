module MyUtil

open FSharp.Core

type ResultBuilder() =
  member this.Bind(f, x) = Result.bind x f
  member this.Return(v) = Result.Ok(v)

let result = new ResultBuilder()
