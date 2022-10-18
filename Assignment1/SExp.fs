namespace Assignment1

type sexp<'a> =
    | Sym of string * 'a
    | Int of int * 'a
    | Bool of bool * 'a
    | Nest of List<sexp<'a>> * 'a

module SExp =

    /// Question 4.
    let rec parse_toks (toks : List<tok<pos>>) : Result<List<sexp<pos>>, string> =
        Ok List.empty

    (*
    and private parse_nested (toks : List<tok<pos>>) =
        match toks with
            | RPAREN _ :: tail ->
                Ok ([], tail)
            | TSym (sym, pos) :: tail ->
                Ok (Sym (sym, pos))
    *)

    let parse_tok tok acc =
        match tok, acc with
            | LPAREN _, _ ->
                Ok ([] :: acc)
            | TSym (sym, pos),  ->
                Ok (Sym (sym, pos) :: acc)
            | TInt (n, pos) ->
                Ok (Int (n, pos) :: acc)
            | TBool (b, pos) ->
                Ok (Bool (b, pos) :: acc)
            | RPAREN pos ->
                match acc with
                    | 