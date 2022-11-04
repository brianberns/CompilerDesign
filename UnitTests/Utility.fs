namespace CompilerDesign.UnitTesting

open Microsoft.VisualStudio.TestTools.UnitTesting
open CompilerDesign.Core

type Assert private () =

    // Improves error message for F# types (e.g. discriminated unions).
    static member AreEqual<'t when 't : equality>(expected : 't, actual : 't) =
        if actual <> expected then
            sprintf "\nExpected: %A.\nActual:   %A" expected actual
                |> Assert.Fail

    // Improves error message for F# types (e.g. discriminated unions).
    static member AreEqual<'t when 't : equality>(expected : 't, actual : 't, msg) =
        if actual <> expected then
            sprintf "%s\nExpected: %A.\nActual:   %A" msg expected actual
                |> Assert.Fail

    // Accepts any F# action.
    static member ThrowsException(action) =
        Microsoft.VisualStudio.TestTools.UnitTesting
            .Assert.ThrowsException(
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
