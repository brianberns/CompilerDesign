namespace CompilerDesign.Core

open System

type ResultBuilder() =
    member _.Return(x) = Ok x
    member _.ReturnFrom(res : Result<_, _>) = res
    member _.Bind(res, f) = Result.bind f res
    member _.Zero() = Ok ()
    member _.Using(disposable : #IDisposable, f) = f disposable

[<AutoOpen>]
module ResultBuilder =

    /// Monadic result builder.
    let result = ResultBuilder()

module Result =

    module List =

        // https://stackoverflow.com/a/53029378/344223
        let traverse f items = 
            let folder head tail =
                result {
                    let! h = f head
                    let! t = tail
                    return h :: t
                }
            let empty = result { return List.empty }
            List.foldBack folder items empty

        let sequence items =
            traverse id items

        // https://hoogle.haskell.org/?hoogle=foldM
        let foldM f state items =

            let rec loop state = function
                | item :: tail ->
                    result {
                        let! state' = f state item
                        return! loop state' tail
                    }
                | [] -> Ok state

            loop state items

/// Standard return type for compiler results. (This is
/// preferable to throwing exceptions.)
type CompilerResult<'a> = Result<'a, string[]>

[<AutoOpen>]
module CompilerResult =

    let error<'a> msg : CompilerResult<'a> =
        Error [| msg |]
