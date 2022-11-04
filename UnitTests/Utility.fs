namespace Microsoft.VisualStudio.TestTools.UnitTesting

open CompilerDesign.Core

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

module Process =

    open System.Diagnostics

    let run assemblyName =
        try
            result {
                let psi =
                    ProcessStartInfo(
                        FileName = "dotnet",
                        Arguments = $"{assemblyName}.dll",
                        RedirectStandardOutput = true)
                use proc = new Process(StartInfo = psi)
                proc.Start() |> ignore
                return proc.StandardOutput
                    .ReadToEnd()
                    .Replace("\r", "")
            }
        with exn -> Error exn.Message
