namespace Assignment3

open FParsec

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
    
    member expr.Tagg =   // F# uses the name "Tag" internally :(
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

module Parser =

    let private (>..>) p1 p2 =
        p1 >>= (fun _ -> p2 >>% ())

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

    let private parseIdentifier =
        identifier (IdentifierOptions ())
            |> parsePos (fun ident tag ->
                IdentifierExpr {|
                    Identifier = ident
                    Tag = tag
                |})

    let private parsePrim1 =
        parse {
            let! op = 
                choice [
                    pstring "add1" >>% Add1
                    pstring "sub1" >>% Sub1
                ]
            do! spaces >..> skipChar '(' >..> spaces
            let! expr = parseExpr
            do! spaces >..> skipChar ')'
            return op, expr
        } |> parsePos (fun (op, expr) tag ->
            Prim1Expr {|
                Operator = op
                Expr = expr
                Tag = tag
            |})

    let private parsePrim2s =
        let create op left right =
            Prim2Expr {|
                Operator = op
                Left = left
                Right = right
                Tag = fst left.Tagg, snd right.Tagg
            |}
        let parseOp =
            choice [
                pchar '+' >>% create Plus
                pchar '-' >>% create Minus
                pchar '*' >>% create Times
            ]
        chainl1
            parseExpr
            (spaces >>. parseOp .>> spaces)

    do parseExprRef.Value <-
        choice [
            parseNumber
            parseIdentifier
            parsePrim1
        ]
