namespace Assignment1

open Microsoft.VisualStudio.TestTools.UnitTesting

module Assert =

    // Improves error message for F# types (e.g. discriminated unions).
    let AreEqual<'t when 't : equality>(expected : 't, actual : 't) =
        if expected <> actual then
            let msg = sprintf "\nExpected: {%A}.\nActual:   {%A}" expected actual
            Assert.Fail(msg)

    // Accepts any F# action.
    let ThrowsException(action) =
        Assert.ThrowsException(
            fun () -> ignore (action ()))
