namespace Assignment1

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
                    let! sexps, pos', tail' = parse_nested pos tail
                    if List.isEmpty sexps then
                        return! Error $"Empty expression at {pos}"
                    else
                        let range =
                            let (startline, startcol, _, _) = pos
                            let (_, _, endline, endcol) = pos'
                            startline, startcol, endline, endcol
                        return Nest (sexps, range), tail'
                | RPAREN pos ->
                    return! Error $"Unmatched right paren at {pos}"
                | TSym (sym, pos) ->
                    return Sym (sym, pos), tail
                | TInt (n, pos) ->
                    return Int (n, pos), tail
                | TBool (b, pos) ->
                    return Bool (b, pos), tail
        }

    and private parse_nested pos toks : Result<List<_> * pos * List<_>, string> =
        result {
            match toks with
                | RPAREN pos' :: tail ->
                    return [], pos', tail
                | tok :: tail ->
                    let! sexp, tail' = parse_sexp tok tail
                    let! sexps, pos', tail'' = parse_nested pos tail'
                    return sexp :: sexps, pos', tail''
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
