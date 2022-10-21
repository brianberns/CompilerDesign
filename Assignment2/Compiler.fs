namespace Assignment2

open System.Diagnostics
open Assignment1

module Compiler =

    let rec convert = function

        | Int (n, pos) ->
            Ok (Number (n, pos))

        | Nest (Sym ("add1", _) :: sexp :: [], pos) ->
            makePrim Add1 sexp pos

        | Nest (Sym ("sub1", _) :: sexp :: [], pos) ->
            makePrim Sub1 sexp pos

        | sexp -> error $"Invalid S-expression: {sexp}"

    and private makePrim op sexp pos =
        result {
            let! e = convert sexp
            return Prim1 (op, e, pos)
        }

    /// Helper function roughly corresponding to function "t"
    /// in the assignment.
    let compile assemblyName text =
        match Assignment1.SExp.parse text with
            | Ok [ sexp ] ->
                result {
                    let! e = convert sexp
                    do! Assembly.compile_prog assemblyName e
                }
            | Ok sexps ->
                error $"Too many S-expressions: ${sexps}"
            | Error msg ->
                error msg

    let run text =
        try
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
        with exn -> error exn.Message
