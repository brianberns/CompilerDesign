namespace CompilerDesign.Assignment3

open System

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

    let rec unparse = function
        | LetExpr rcd ->
            let bindings =
                let exprs =
                    rcd.Bindings
                        |> Seq.map (fun binding ->
                            $"{binding.Identifier} = {unparse binding.Expr}")
                String.Join(", ", exprs)
            $"let {bindings} in {rcd.Expr}"
        | Prim1Expr rcd ->
            let op = (string rcd.Operator).ToLower()
            $"op({unparse rcd.Expr})"
        | Prim2Expr rcd ->
            let op = function
                | Plus -> '+'
                | Minus -> '-'
                | Times -> '*'
            $"{rcd.Left} {op} {rcd.Right}"
        | IfExpr rcd ->
            $"if {unparse rcd.Condition} : \
                {unparse rcd.TrueBranch} \
                else: {unparse rcd.FalseBranch}"
        | NumberExpr rcd -> string rcd.Number
        | IdentifierExpr rcd -> rcd.Identifier
