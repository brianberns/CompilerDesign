namespace CompilerDesign.Assignment6

open CompilerDesign.Core

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
        Type : Type<'tag>
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

module private rec Unparse =

    let private unparseTypeArgs typeArgs =
        if typeArgs |> List.isEmpty then ""
        else
            let str =
                typeArgs
                    |> List.map Type.unparse
                    |> String.concat ", "
            $"<{str}>"

    let private unparseBinding (binding : Binding<_>) =
        let ident = binding.Identifier.Name
        let expr = unparseExpr binding.Expr
        let typ =
            match binding.Type with
                | TypeBlank _ -> ""
                | t -> $" : {Type.unparse t}"
        $"{ident}{typ} = {expr}"

    let unparseExpr = function
        | LetExpr def ->
            let bindings =
                def.Bindings
                    |> Seq.map unparseBinding
                    |> String.concat ", "
            $"(let {bindings} in {unparseExpr def.Expr})"
        | Prim1Expr def ->
            let op = function
                | Not -> "!"
                | prim1 -> (string prim1).ToLower()
            $"{op def.Operator}{unparseTypeArgs def.TypeArguments}({unparseExpr def.Expr})"
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
            $"({unparseExpr def.Left} \
                {op def.Operator}{unparseTypeArgs def.TypeArguments} \
                {unparseExpr def.Right})"
        | IfExpr def ->
            $"(if {unparseExpr def.Condition} : \
                {unparseExpr def.TrueBranch} \
                else: {unparseExpr def.FalseBranch})"
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
                    |> Seq.map unparseExpr
                    |> String.concat ", "
            $"{def.Identifier.Name}{unparseTypeArgs def.TypeArguments}({args})"
        | AnnotationExpr def ->
            $"({unparseExpr def.Expr} : {Type.unparse def.Type})"

module private rec TypeCheck =

    let private intName = "Int"
    let private boolName = "Bool"

    let typeOfExpr env : Expr<'tag> -> Result<Type<'tag>, _> = function
        | LetExpr def -> typeOfLet env def
        | Prim1Expr def -> typeOfPrim1 env def
        | Prim2Expr def -> Ok (TypeBlank def.Tag)
        | IfExpr def -> Ok (TypeBlank def.Tag)
        | NumberExpr def ->
            Ok (TypeConstant { Name = intName; Tag = def.Tag })
        | IdentifierExpr def -> Ok (TypeBlank def.Tag)
        | BoolExpr def ->
            Ok (TypeConstant { Name = boolName; Tag = def.Tag })
        | ApplicationExpr def -> Ok (TypeBlank def.Tag)
        | AnnotationExpr def -> Ok (TypeBlank def.Tag)

    let private typeOfLet env (def : LetDef<_>) =
        typeOfExpr env def.Expr

    let private typeOfPrim1 env def =

        let check expected =
            result {
                let! actual = typeOfExpr env def.Expr
                match actual with
                    | TypeConstant ident
                        when ident.Name = expected ->
                            return actual
                    | _ ->
                        return! Error $"Expected: {expected}, Actual: {Type.unparse actual}"
            }

        match def.Operator with
            | Add1 | Sub1 -> check intName
            | Not -> check boolName
            | Print
            | IsBool
            | IsNum -> typeOfExpr env def.Expr

module Expr =

    let unparse = Unparse.unparseExpr

    let typeOf = TypeCheck.typeOfExpr
