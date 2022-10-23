﻿namespace CompilerDesign.Assignment2

open CompilerDesign.Core
open CompilerDesign.Assignment1   // for S-expression parser

module Compiler =

    /// Converts an S-expression into an Adder expression.
    let rec convert = function

        | Int (n, pos) ->
            Ok (Number (n, pos))

        | Nest ([Sym ("add1", _); sexp], pos) ->
            makePrim Add1 sexp pos

        | Nest ([Sym ("sub1", _); sexp], pos) ->
            makePrim Sub1 sexp pos

        | Nest ([Sym ("let", _); Nest (sexps, _); sexp], pos) ->
            makeLet sexps sexp pos

        | Sym (name, pos) ->
            Ok (Id (name, pos))

        | sexp -> error $"Invalid S-expression: {sexp}"

    and private makePrim op sexp pos =
        result {
            let! e = convert sexp
            return Prim1 (op, e, pos)
        }

    and private makeLet sexps sexp pos =

        let rec makeBindings sexps =
            match sexps with
                | Nest ([Sym (name, _ : pos); sexp], _) :: tail ->
                    result {
                        let! exp = convert sexp
                        let! bindings = makeBindings tail
                        return (name, exp) :: bindings
                    }
                | [] -> Ok []
                | sexp :: _ -> error $"Unexpected binding: {sexp}"

        result {
            let! bindings = makeBindings sexps
            let! exp = convert sexp
            return Let (bindings, exp, pos)
        }

    /// Helper function roughly corresponding to function "t"
    /// in the assignment.
    let compile assemblyName text =
        match SExp.parse text with
            | Ok [ sexp ] ->
                result {
                    let! e = convert sexp
                    let! node = Expr.compile e
                    do! Compiler.compile_prog assemblyName node
                }
            | Ok sexps ->
                error $"Too many S-expressions: ${sexps}"
            | Error msg ->
                error msg
