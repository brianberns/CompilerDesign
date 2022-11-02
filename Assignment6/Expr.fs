﻿namespace CompilerDesign.Assignment6

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

type NumberDef<'tag> =
    {
        Number : int
        Tag : 'tag
    }

type BoolDef<'tag> =
    {
        Flag : bool
        Tag : 'tag
    }

type Expr<'tag> =
    | LetExpr of LetDef<'tag>
    | Prim1Expr of Prim1Def<'tag>
    | Prim2Expr of Prim2Def<'tag>
    | IfExpr of IfDef<'tag>
    | NumberExpr of NumberDef<'tag>   // numeric literal
    | IdentifierExpr of IdentifierDef<'tag>
    | BoolExpr of BoolDef<'tag>       // Boolean literal
    | ApplicationExpr of ApplicationDef<'tag>
    | AnnotationExpr of AnnotationDef<'tag>
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
            | ApplicationExpr def -> def.Tag
            | AnnotationExpr def -> def.Tag

and LetDef<'tag> =
    {
        Bindings : List<Binding<'tag>>
        Expr : Expr<'tag>
        Tag : 'tag
    }

and Binding<'tag> =
    {
        Identifier : IdentifierDef<'tag>
        Expr : Expr<'tag>
    }

and Prim1Def<'tag> =
    {
        Operator : Prim1
        TypeArguments : List<Type<'tag>>
        Expr : Expr<'tag>
        Tag : 'tag
    }

and Prim2Def<'tag> =
    {
        Operator : Prim2
        TypeArguments : List<Type<'tag>>
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

and ApplicationDef<'tag> =
    {
        /// Name of function being called.
        Identifier : IdentifierDef<'tag>
        TypeArguments : List<Type<'tag>>
        Arguments : List<Expr<'tag>>
        Tag : 'tag
    }

and AnnotationDef<'tag> =
    {
        Expr : Expr<'tag>
        Type: Type<'tag>
        Tag : 'tag
    }

module Expr =

    let unparseTypeArgs typeArgs =
        if typeArgs |> List.isEmpty then ""
        else
            let str =
                typeArgs
                    |> List.map Type.unparse
                    |> String.concat ", "
            $"<{str}>"

    let rec unparse = function
        | LetExpr def ->
            let bindings =
                def.Bindings
                    |> Seq.map (fun binding ->
                        $"{binding.Identifier.Name} = {unparse binding.Expr}")
                    |> String.concat ", "
            $"(let {bindings} in {unparse def.Expr})"
        | Prim1Expr def ->
            let op = function
                | Not -> "!"
                | prim1 -> (string prim1).ToLower()
            $"{op def.Operator}{unparseTypeArgs def.TypeArguments}({unparse def.Expr})"
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
            $"({unparse def.Left} {op def.Operator}{unparseTypeArgs def.TypeArguments} {unparse def.Right})"
        | IfExpr def ->
            $"(if {unparse def.Condition} : \
                {unparse def.TrueBranch} \
                else: {unparse def.FalseBranch})"
        | NumberExpr def ->
            if def.Number < 0 then
                $"({def.Number})"   // use parens to avoid ambiguity
            else
                $"{def.Number}"
        | IdentifierExpr def -> def.Name
        | BoolExpr def -> (string def.Flag).ToLower()
        | ApplicationExpr def ->
            let args =
                def.Arguments
                    |> Seq.map unparse
                    |> String.concat ", "
            $"{def.Identifier.Name}({args})"
        | AnnotationExpr def ->
            $"({unparse def.Expr} : {Type.unparse def.Type})"
