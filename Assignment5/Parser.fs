namespace CompilerDesign.Assignment5

open System
open FParsec
open CompilerDesign.Core

module Parser =

    let private skipComment =
        skipChar '#'
            >>. skipManyTill
                anyChar
                (skipNewline <|> eof)

    let private spaces =
        skipMany (
            skipSatisfy Char.IsWhiteSpace
                <|> skipComment)

    let private parsePos create parser =
        parse {
            let! startPos = getPosition
            let! value = parser
            let! endPos = getPosition
            return create value (startPos, endPos)
        }

    let private parseIdentifierDef =
        identifier (IdentifierOptions ())
            |> parsePos (fun name tag ->
                {
                    Name = name
                    Tag = tag
                })

    let private choiceF pairs =
        pairs
            |> Seq.map (fun (str, f) ->
                skipString str >>% f)
            |> choice

    let private parseParens parser =
        parse {
            do! skipChar '(' >>. spaces
            let! value = parser
            do! spaces >>. skipChar ')'
            return value
        }

    module private Expr =

        let private parseExpr, parseExprRef =
            createParserForwardedToRef ()

        let private parseNumber : Parser<_, unit> =
            pint32
                |> parsePos (fun n tag ->
                    NumberExpr {
                        Number = n
                        Tag = tag
                    })

        let private parseIdentifier =
            parseIdentifierDef
                |>> IdentifierExpr

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
                let! expr = parseParens parseExpr
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
                let! ident = parseIdentifierDef
                do! spaces >>. skipChar '=' >>. spaces
                let! expr = parseExpr
                return {
                    Identifier = ident
                    Expr = expr
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

        let private parseArguments =
            sepBy
                (parseExpr .>> spaces)
                (skipChar ',' >>. spaces)

        let private parseApplication =
            parse {
                let! ident = parseIdentifierDef
                do! spaces
                let! args = parseParens parseArguments
                return ApplicationExpr {
                    Identifier = ident
                    Arguments = args
                }
            } |> attempt          // rollback if needed

        let private parseSimpleExpr =
            choice [
                parseNumber
                parseBool
                parsePrim1
                parseIf
                parseLet
                parseApplication
                parseIdentifier   // must come after other parsers
                parseParens parseExpr
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

        let parse = parseExpr

        do parseExprRef.Value <- parseExprImpl

    module private Decl =

        let private parseParameters =
            sepBy
                (parseIdentifierDef .>> spaces)
                (skipChar ',' >>. spaces)

        let parse =
            parse {
                do! skipString "def" >>. spaces
                let! ident = parseIdentifierDef
                do! spaces
                let! parms = parseParens parseParameters
                do! spaces >>. skipChar ':' >>. spaces
                let! body = Expr.parse
                return {
                    Identifier = ident
                    Parameters = parms
                    Body = body
                }
            }

    let private parseText =
        spaces
            >>. Expr.parse
            .>> spaces
            .>> eof

    let parse text =
        match runParserOnString parseText () "" text with
            | Success (result, _, _) -> Result.Ok result
            | Failure (msg, _, _) -> CompilerResult.error msg
