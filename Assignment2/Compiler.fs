namespace Assignment2

open System.Diagnostics
open Assignment1

module Compiler =

    let rec convert = function

        | Int (n, pos) :: [] ->
            Ok (Number (n, pos))

        | Sym ("add1", startpos) :: Nest (sexps, endpos) :: [] ->
            makePrim Add1 sexps startpos endpos

        | Sym ("sub1", startpos) :: Nest (sexps, endpos) :: [] ->
            makePrim Sub1 sexps startpos endpos

        | sexps -> Error [| $"Invalid S-expressions: {sexps}" |]

    and private makePrim op sexps startpos endpos =
        result {
            let! e = convert sexps
            let range = Pos.range startpos endpos
            return Prim1 (op, e, range)
        }

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
