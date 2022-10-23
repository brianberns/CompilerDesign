﻿namespace CompilerDesign.Assignment3

open FParsec
open CompilerDesign.Core

type Prim1 =
    | Add1
    | Sub1

type Prim2 =
    | Plus
    | Minus
    | Times

type Expr<'tag> =
    | LetExpr of
        {|
            Bindings : List<Binding<'tag>>
            Expr : Expr<'tag>
            Tag : 'tag
        |}
    | Prim1Expr of
        {|
            Operator : Prim1
            Expr : Expr<'tag>
            Tag : 'tag
        |}
    | Prim2Expr of
        {|
            Operator : Prim2
            Left : Expr<'tag>
            Right : Expr<'tag>
            Tag : 'tag
        |}
    | IfExpr of
        {|
            Condition : Expr<'tag>
            TrueBranch : Expr<'tag>
            FalseBranch : Expr<'tag>
            Tag : 'tag
        |}
    | NumberExpr of
        {|
            Number : int
            Tag : 'tag
        |}
    | IdentifierExpr of
        {|
            Identifier : string
            Tag : 'tag
        |}

    with
    
    member expr.Tag' =   // F# uses the name "Tag" internally :(
        match expr with
            | LetExpr x -> x.Tag
            | Prim1Expr x -> x.Tag
            | Prim2Expr x -> x.Tag
            | IfExpr x -> x.Tag
            | NumberExpr x -> x.Tag
            | IdentifierExpr x -> x.Tag

and Binding<'tag> =
    {
        Identifier : string
        Expr : Expr<'tag>
        Tag : 'tag
    }

module Expr =

    let private parseExpr, parseExprRef =
        createParserForwardedToRef ()

    let private parsePos create parser =
        parse {
            let! startPos = getPosition
            let! value = parser
            let! endPos = getPosition
            return create value (startPos, endPos)
        }

    let private parseNumber : Parser<_, unit> =
        pint32
            |> parsePos (fun n tag ->
                NumberExpr {|
                    Number = n
                    Tag = tag
                |})

    let private parseIdentifierName =
        identifier (IdentifierOptions ())
            |> parsePos (fun ident tag ->
                (ident, tag))

    let private parseIdentifier =
        parseIdentifierName
            |>> (fun (ident, tag) ->
                IdentifierExpr {|
                    Identifier = ident
                    Tag = tag
                |})

    let private parseParens =
        parse {
            do! skipChar '(' >>. spaces
            let! expr = parseExpr
            do! spaces >>. skipChar ')'
            return expr
        }

    let private parsePrim1 =
        parse {
            let! op = 
                choice [
                    skipString "add1" >>% Add1
                    skipString "sub1" >>% Sub1
                ]
            do! spaces
            let! expr = parseParens
            return op, expr
        } |> parsePos (fun (op, expr) tag ->
            Prim1Expr {|
                Operator = op
                Expr = expr
                Tag = tag
            |})

    let private parseIf =
        parse {
            do! skipString "if" >>. spaces
            let! cond = parseExpr
            do! spaces >>. skipChar ':' >>. spaces
            let! trueBranch = parseExpr
            do! spaces >>. skipString "else:" >>. spaces
            let! falseBranch = parseExpr
            return cond, trueBranch, falseBranch
        } |> parsePos (fun (cond, trueBranch, falseBranch) tag ->
            IfExpr {|
                Condition = cond
                TrueBranch = trueBranch
                FalseBranch = falseBranch
                Tag = tag
            |})

    let private parseBinding =
        parse {
            let! (ident, tag) = parseIdentifierName
            do! spaces >>. skipChar '=' >>. spaces
            let! expr = parseExpr
            return {
                Identifier = ident
                Expr = expr
                Tag = tag   // identifier tag, not for entire binding
            }
        }

    let private parseBindings =
        sepBy1
            (parseBinding .>> spaces)
            (skipChar ',' .>> spaces)

    let private parseLet =
        parse {
            do! skipString "let" >>. spaces
            let! bindings = parseBindings
            do! spaces >>. skipString "in" >>. spaces
            let! expr = parseExpr
            return bindings, expr
        } |> parsePos (fun (bindings, expr) tag ->
            LetExpr {|
                Bindings = bindings
                Expr = expr
                Tag = tag
            |})

    let private parseSimpleExpr =
        choice [
            parseNumber
            parsePrim1
            parseIf
            parseLet
            parseIdentifier   // must come after any parser that looks for keywords
            parseParens
        ]

    let private parseExprImpl =
        let create op left right =
            Prim2Expr {|
                Operator = op
                Left = left
                Right = right
                Tag = fst left.Tag', snd right.Tag'
            |}
        let parseOp =
            choice [
                pchar '+' >>% create Plus
                pchar '-' >>% create Minus
                pchar '*' >>% create Times
            ]
        chainl1
            (parseSimpleExpr .>> spaces)
            (parseOp .>> spaces)

    let private parseText =
        spaces
            >>. parseExpr
            .>> spaces
            .>> eof

    let parse text =
        match runParserOnString parseText () "" text with
            | Success (result, _, _) -> Result.Ok result
            | Failure (msg, _, _) -> CompilerResult.error msg

    do parseExprRef.Value <- parseExprImpl
