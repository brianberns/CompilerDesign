namespace CompilerDesign.Assignment4

open FParsec
open CompilerDesign.Core

module Parser =

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
                NumberExpr {
                    Number = n
                    Tag = tag
                })

    let private parseIdentifierName =
        identifier (IdentifierOptions ())
            |> parsePos (fun ident tag ->
                (ident, tag))

    let private parseIdentifier =
        parseIdentifierName
            |>> (fun (ident, tag) ->
                IdentifierExpr {
                    Identifier = ident
                    Tag = tag
                })

    let private choiceF pairs =
        pairs
            |> Seq.map (fun (str, f) ->
                skipString str >>% f)
            |> choice

    let private parseBool =
        choiceF [
            "true", true
            "false", false
        ]
            |> parsePos (fun flag tag ->
                BoolExpr {
                    Flag = flag
                    Tag = tag
                })

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
                choiceF [
                    "add1", Add1
                    "sub1", Sub1
                    "print", Print
                    "isbool", IsBool
                    "isnum", IsNum
                    "!", Not
                ]
            do! spaces
            let! expr = parseParens
            return op, expr
        } |> parsePos (fun (op, expr) tag ->
            Prim1Expr {
                Operator = op
                Expr = expr
                Tag = tag
            })

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
            IfExpr {
                Condition = cond
                TrueBranch = trueBranch
                FalseBranch = falseBranch
                Tag = tag
            })

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
            LetExpr {
                Bindings = bindings
                Expr = expr
                Tag = tag
            })

    let private parseSimpleExpr =
        choice [
            parseNumber
            parseBool
            parsePrim1
            parseIf
            parseLet
            parseIdentifier   // must come after any parser that looks for keywords
            parseParens
        ]

    let private parseExprImpl =
        let create op left right =
            Prim2Expr {
                Operator = op
                Left = left
                Right = right
                Tag = fst left.Tag', snd right.Tag'
            }
        let parseOp =
            choiceF [
                "+", create Plus
                "-", create Minus
                "*", create Times
                "&&", create And
                "||", create Or
                ">=", create GreaterEq   // must come before ">"
                ">", create Greater
                "<=", create LessEq      // must come before "<"
                "<", create Less
                "==", create Eq
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
