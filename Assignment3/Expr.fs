namespace CompilerDesign.Assignment3

type Prim1 =
    | Add1
    | Sub1

type Prim2 =
    | Plus
    | Minus
    | Times

type Expr<'tag> =
    | LetExpr of LetDef<'tag>
    | Prim1Expr of Prim1Def<'tag>
    | Prim2Expr of Prim2Def<'tag>
    | IfExpr of IfDef<'tag>
    | NumberExpr of NumberDef<'tag>
    | IdentifierExpr of IdentifierDef<'tag>
    with
    
    member expr.Tag' =   // F# uses the name "Tag" internally :(
        match expr with
            | LetExpr x -> x.Tag
            | Prim1Expr x -> x.Tag
            | Prim2Expr x -> x.Tag
            | IfExpr x -> x.Tag
            | NumberExpr x -> x.Tag
            | IdentifierExpr x -> x.Tag

and LetDef<'tag> =
    {
        Bindings : List<Binding<'tag>>
        Expr : Expr<'tag>
        Tag : 'tag
    }

and Binding<'tag> =
    {
        Identifier : string
        Expr : Expr<'tag>
        Tag : 'tag
    }

and Prim1Def<'tag> =
    {
        Operator : Prim1
        Expr : Expr<'tag>
        Tag : 'tag
    }

and Prim2Def<'tag> =
    {
        Operator : Prim2
        Left : Expr<'tag>
        Right : Expr<'tag>
        Tag : 'tag
    }

and IfDef<'tag> =
    {
        Condition : Expr<'tag>
        TrueBranch : Expr<'tag>
        FalseBranch : Expr<'tag>
        Tag : 'tag
    }

and NumberDef<'tag> =
    {
        Number : int
        Tag : 'tag
    }

and IdentifierDef<'tag> =
    {
        Identifier : string
        Tag : 'tag
    }

module Expr =

    let rec unparse = function
        | LetExpr def ->
            let bindings =
                def.Bindings
                    |> Seq.map (fun binding ->
                        $"{binding.Identifier} = {unparse binding.Expr}")
                    |> String.concat ", "
            $"(let {bindings} in {unparse def.Expr})"
        | Prim1Expr def ->
            let op = (string def.Operator).ToLower()
            $"{op}({unparse def.Expr})"
        | Prim2Expr def ->
            let op = function
                | Plus -> '+'
                | Minus -> '-'
                | Times -> '*'
            $"({unparse def.Left} {op def.Operator} {unparse def.Right})"
        | IfExpr def ->
            $"(if {unparse def.Condition} : \
                {unparse def.TrueBranch} \
                else: {unparse def.FalseBranch})"
        | NumberExpr def -> string def.Number
        | IdentifierExpr def -> def.Identifier
