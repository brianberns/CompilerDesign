﻿namespace Assignment1

open FParsec

type tok<'a> =
    | LPAREN of 'a
    | RPAREN of 'a
    | TSym of string * 'a
    | TInt of int * 'a
    | TBool of bool * 'a

(* startline, startcol, endline, endcol *)
type pos = int * int * int * int

module Tok =

    let private parsePos p makeToken : Parser<_, unit> =
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
            parseSym   // must follow parseBool to avoid parsing Boolean literals as symbols
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
