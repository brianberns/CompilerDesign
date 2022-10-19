namespace Assignment1

open FParsec

type tok<'a> =
    | LPAREN of 'a
    | RPAREN of 'a
    | TSym of string * 'a
    | TInt of int * 'a
    | TBool of bool * 'a

(* startline, startcol, endline, endcol *)
type pos = int * int * int * int   // this is really ugly, but we use what we're given

module Tok =

    (*
    Use FParsec to tokenize, since we don't have access to the original
    OCaml tokenizer.
    *)

    let private parsePos p makeToken =
        parse {
            let! startPos = getPosition
            let! value = p
            let! endPos = getPosition
            let pos : pos =
                int startPos.Line - 1,
                int startPos.Column - 1,
                int endPos.Line - 1,
                int endPos.Column - 1
            return makeToken (value, pos)
        }

    let private parseLparen =
        parsePos
            (skipChar '(')
            (snd >> LPAREN)

    let private parseRparen =
        parsePos
            (skipChar ')')
            (snd >> RPAREN)

    let private parseSym =
        let psym = identifier (IdentifierOptions ())
        parsePos psym TSym

    let private parseInt =
        parsePos pint32 TInt

    let private parseBool =
        let pbool =
            choice [
                skipStringCI "true" >>% true
                skipStringCI "false" >>% false
            ]
        parsePos pbool TBool

    let private parseTok =
        choice [
            parseLparen
            parseRparen
            parseInt
            parseBool
            parseSym   // last to avoid parsing literals as symbols (e.g. "true")
        ] .>> spaces

    let private parseText =
        spaces
            >>. many parseTok
            .>> spaces
            .>> eof

    /// Question 3.
    let tokenize text =
        match runParserOnString parseText () "" text with
            | Success (result, _, _) -> result
            | Failure (msg, _, _) -> failwith msg
