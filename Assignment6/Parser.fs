﻿namespace CompilerDesign.Assignment6

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

    let private parseCsv parser =
        sepBy
            (parser .>> spaces)
            (skipChar ',' .>> spaces)

    let private parseCsv1 parser =
        sepBy1
            (parser .>> spaces)
            (skipChar ',' .>> spaces)

    module private Type =

        let private parseType, parseTypeRef =
            createParserForwardedToRef ()

        /// Allow "_" to indicate a blank type. This isn't specified in
        /// the assignment, but is useful for inputs like (x : _).
        let private parseBlank =
            skipChar '_'
                |> parsePos (fun () tag ->
                    TypeBlank tag)

        let private parseConstant =
            parseIdentifierDef |>> TypeConstant

        let private parseVariable =
            skipChar '\''
                >>. parseIdentifierDef
                |>> TypeVariable   // don't store apostrophe

        let private parseFunction =
            parse {
                let! inputs = parseCsv1 parseType
                do! spaces >>. skipString "->" >>. spaces
                let! output = parseType
                return inputs, output
            }
                |> parseParens
                |> parsePos (fun (inputs, output) tag ->
                    TypeArrow {
                        InputTypes = inputs
                        OutputType = output
                        Tag = tag
                    })

        let private parseTypeImpl =
            choice [
                parseBlank
                parseConstant
                parseVariable
                parseFunction
            ]

        let parse = parseType

        do parseTypeRef.Value <- parseTypeImpl

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
                    TypeArguments = []
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
            parseCsv1 parseBinding

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
            parseCsv parseExpr

        let private parseApplication =
            parse {
                let! ident = parseIdentifierDef
                // no spaces allowed here!
                let! args = parseParens parseArguments
                return ident, args
            }
                |> parsePos (fun (ident, args) tag ->
                    ApplicationExpr {
                        Identifier = ident
                        TypeArguments = []
                        Arguments = args
                        Tag = tag
                    })
                |> attempt          // rollback if needed

        let private parseParenExpr =
            parseExpr
                |> parseParens
                |> attempt          // rollback if needed

        let private parseAnnotation =
            parse {
                let! expr = parseExpr
                do! spaces >>. skipChar ':' >>. spaces
                let! typ = Type.parse
                return expr, typ
            }
                |> parseParens
                |> parsePos (fun (expr, typ) tag ->
                    AnnotationExpr {
                        Expr = expr
                        Type = typ
                        Tag = tag
                    })

        let private parseSimpleExpr =
            choice [
                parseNumber
                parseBool
                parsePrim1
                parseIf
                parseLet
                parseApplication
                parseIdentifier   // must come after other parsers
                parseParenExpr
                parseAnnotation
            ]

        let private parseExprImpl =
            let create op left right =
                Prim2Expr {
                    Operator = op
                    TypeArguments = []
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
            parseCsv parseIdentifierDef

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

    module private Program =

        let parse =
            parse {
                do! spaces
                let! decls =
                    many (Decl.parse .>> spaces)
                let! main = Expr.parse .>> spaces
                do! eof
                return {
                    Declarations = decls
                    Main = main
                }
            }

    let parse text =
        match runParserOnString Program.parse () "" text with
            | Success (result, _, _) -> Result.Ok result
            | Failure (msg, _, _) -> CompilerResult.error msg
