module MyUtil

open FSharp.Core


type ResultBuilder() =
  member this.Bind(f, x) = Result.bind x f
  member this.Return(v) = Result.Ok(v)


let result = new ResultBuilder()


module List =

  let foldM<'a, 'b, 'e> (f : 'a -> 'b -> Result<'a, 'e>) (init : Result<'a, 'e>) (xs : 'b list) : Result<'a, 'e> =
    xs |> List.fold (fun res x ->
      result {
        let! acc = res
        let! v = f acc x
        return v
      }
    ) init


  let mapM<'a, 'b, 'e> (f : 'a -> Result<'b, 'e>) (xs : 'a list) : Result<'b list, 'e> =
    result {
      let! acc =
        xs |> List.fold (fun res x ->
          result {
            let! acc = res
            let! v = f x
            return (v :: acc)
          }
        ) (Ok([]))
      return (List.rev acc)
    }
