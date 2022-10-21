namespace Assignment2

open System.Diagnostics
open Assignment1

module Compiler =

    let rec convert = function

        | Int (n, _) :: [] ->
            Ok (expr.Num n)

        | Sym ("add1", _) :: Nest (sexps, _) :: [] ->
            result {
                let! e = convert sexps
                return Add1 e
            }

        | Sym ("sub1", _) :: Nest (sexps, _) :: [] ->
            result {
                let! e = convert sexps
                return Sub1 e
            }

        | sexps -> Error [| $"Invalid S-expressions: {sexps}" |]

    /// Helper function roughly corresponding to function "t"
    /// in the assignment.
    let compile assemblyName text =
        match Assignment1.SExp.parse text with
            | Ok sexps ->
                result {
                    let! e = convert sexps
                    do! Assembly.compile_prog assemblyName e
                }
            | Error msg ->
                Error [| msg |]

    let run text =
        let assemblyName = "Adder"
        result {
            do! compile assemblyName text

            let psi =
                ProcessStartInfo(
                    FileName = "dotnet",
                    Arguments = $"{assemblyName}.dll",
                    RedirectStandardOutput = true)
            use proc = new Process(StartInfo = psi)
            proc.Start() |> ignore
            return proc.StandardOutput.ReadToEnd()
        }
