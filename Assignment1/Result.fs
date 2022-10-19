namespace Assignment1

type ResultBuilder() =
    member _.Return(x) = Ok x
    member _.ReturnFrom(res : Result<_, _>) = res
    member _.Bind(res, f) = Result.bind f res

[<AutoOpen>]
module ResultBuilder =
    let result = ResultBuilder()
