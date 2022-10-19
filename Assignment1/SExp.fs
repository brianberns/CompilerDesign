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

    /// Parses a single expression starting at the given token.
    let rec private parse_sexp tok tail =
        result {
            match tok with

                    // parse sub-expressions until the corresponding r-paren
                | LPAREN (pos : pos) ->
                    let! sexps, pos', tail' = parse_nested pos tail
                    let range =
                        let (startline, startcol, _, _) = pos
                        let (_, _, endline, endcol) = pos'
                        startline, startcol, endline, endcol
                    return Nest (sexps, range), tail'

                    // unexpected r-paren
                | RPAREN pos ->
                    let line, col, _ , _ = pos
                    return! Error $"Unmatched right paren at line {line}, col {col}"

                | TSym (sym, pos) -> return Sym (sym, pos), tail
                | TInt (n, pos) -> return Int (n, pos), tail
                | TBool (b, pos) -> return Bool (b, pos), tail
        }

    /// Parses nested sub-expressions.
    and private parse_nested pos toks =
        result {
            match toks with

                    // no sub-expressions to parse
                | RPAREN (pos' : pos) :: tail ->
                    return [], pos', tail

                    // parse sub-expression starting at the current token
                | tok :: tail ->
                    let! sexp, tail' = parse_sexp tok tail
                    let! sexps, pos', tail'' = parse_nested pos tail'
                    return sexp :: sexps, pos', tail''

                    // ran out of tokens
                | [] ->
                    let line, col, _ , _ = pos
                    return! Error $"Unmatched left paren at line {line}, col {col}"
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
