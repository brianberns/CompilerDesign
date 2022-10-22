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

/// Standard return type for compiler results. (This is
/// preferable to throwing exceptions.)
type CompilerResult<'a> = Result<'a, string[]>

[<AutoOpen>]
module CompilerResult =

    let error<'a> msg : CompilerResult<'a> =
        Error [| msg |]
