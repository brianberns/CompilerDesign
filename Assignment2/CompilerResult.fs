namespace Assignment2

/// Standard return type for compiler results. (This is
/// preferable to throwing exceptions.)
type CompilerResult<'a> = Result<'a, string[]>

[<AutoOpen>]
module private CompilerResult =

    let error<'a> msg : CompilerResult<'a> =
        Error [| msg |]
