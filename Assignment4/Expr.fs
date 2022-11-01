namespace CompilerDesign.Assignment4

type Prim1 =
    | Add1
    | Sub1
    | Print
    | IsBool
    | IsNum
    | Not

type Prim2 =
    | Plus
    | Minus
    | Times
    | And
    | Or
    | Greater
    | GreaterEq
    | Less
    | LessEq
    | Eq

type Expr<'tag> =
    | LetExpr of LetDef<'tag>
    | Prim1Expr of Prim1Def<'tag>
    | Prim2Expr of Prim2Def<'tag>
    | IfExpr of IfDef<'tag>
    | NumberExpr of NumberDef<'tag>   // numeric literal
    | IdentifierExpr of IdentifierDef<'tag>
    | BoolExpr of BoolDef<'tag>       // Boolean literal
    with
    
    member expr.Tag' =   // F# uses the name "Tag" internally :(
        match expr with
            | LetExpr def -> def.Tag
            | Prim1Expr def -> def.Tag
            | Prim2Expr def -> def.Tag
            | IfExpr def -> def.Tag
            | NumberExpr def -> def.Tag
            | IdentifierExpr def -> def.Tag
            | BoolExpr def -> def.Tag

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

and BoolDef<'tag> =
    {
        Flag : bool
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
            let op = function
                | Not -> "!"
                | prim1 -> (string prim1).ToLower()
            $"{op def.Operator}({unparse def.Expr})"
        | Prim2Expr def ->
            let op = function
                | Plus -> "+"
                | Minus -> "-"
                | Times -> "*"
                | And -> "&&"
                | Or -> "||"
                | Greater -> ">"
                | GreaterEq -> ">="
                | Less -> "<"
                | LessEq -> "<="
                | Eq -> "=="
            $"({unparse def.Left} {op def.Operator} {unparse def.Right})"
        | IfExpr def ->
            $"(if {unparse def.Condition} : \
                {unparse def.TrueBranch} \
                else: {unparse def.FalseBranch})"
        | NumberExpr def -> string def.Number
        | IdentifierExpr def -> def.Identifier
        | BoolExpr def -> (string def.Flag).ToLower()
