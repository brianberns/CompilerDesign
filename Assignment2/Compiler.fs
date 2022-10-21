namespace Assignment2

open Assignment1

module Compiler =

    // Question 1.
    let compile e =
        Expr.compile_expr e

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
    let run text =

        match Assignment1.SExp.parse text with
            | Ok sexps ->
                result {
                    let! e = convert sexps
                    let diagnostics = Assembly.compile_prog "Adder" e
                    if diagnostics.Length = 0 then
                        return 0
                    else
                        return! Error diagnostics
                }
            | Error msg ->
                Error [| msg |]
