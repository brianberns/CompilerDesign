﻿namespace Assignment1

type ResultBuilder() =
    member _.Return(x) = Ok x
    member _.ReturnFrom(res) = res
    member _.Bind(x, f) = Result.bind f x

[<AutoOpen>]
module ResultBuilder =
    let result = ResultBuilder()

type sexp<'a> =
    | Sym of string * 'a
    | Int of int * 'a
    | Bool of bool * 'a
    | Nest of List<sexp<'a>> * 'a

module SExp =

    let getPos = function
        | Sym (_, pos) -> pos
        | Int (_, pos) -> pos
        | Bool (_, pos) -> pos
        | Nest (_, pos) -> pos

    let rec private parse_sexp tok tail =
        result {
            match tok with
                | LPAREN pos ->
                    let! sexps, tail' = parse_sexps pos tail
                    if List.isEmpty sexps then
                        return! Error $"Empty expression at {pos}"
                    else
                        let pos' =
                            let (startline, startcol, _, _) = pos
                            let (_, _, endline, endcol) = getPos (List.last sexps)
                            startline, startcol, endline, endcol
                        return Nest (sexps, pos'), tail'
                | RPAREN pos ->
                    return! Error $"Unmatched right paren at {pos}"
                | TSym (sym, pos) ->
                    return Sym (sym, pos), tail
                | TInt (n, pos) ->
                    return Int (n, pos), tail
                | TBool (b, pos) ->
                    return Bool (b, pos), tail
        }

    and private parse_sexps pos toks =
        result {
            match toks with
                | RPAREN pos :: tail ->
                    return [], tail
                | tok :: tail ->
                    let! sexp, tail' = parse_sexp tok tail
                    let! sexps, tail'' = parse_sexps pos tail'
                    return sexp :: sexps, tail''
                | [] ->
                    return! Error $"Unmatched left paren at {pos}"
        }

    /// Question 4.
    let rec parse_toks toks =
        result {
            match toks with
                | tok :: tail ->
                    let! sexp, tail' = parse_sexp tok tail
                    let! sexps = parse_toks tail'
                    return sexp :: sexps
                | [] -> return List.empty
        }
