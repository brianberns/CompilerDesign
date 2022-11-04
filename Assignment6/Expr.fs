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

module Expr =

    let rec untag = function
        | LetExpr def->
            LetExpr {
                Bindings =
                    def.Bindings
                        |> List.map (fun binding ->
                            {
                                Identifier = IdentifierDef.untag binding.Identifier
                                Type = Type.untag binding.Type
                                Expr = untag binding.Expr
                            })
                Expr = untag def.Expr
                Tag = ()
            }
        | Prim1Expr def ->
            Prim1Expr {
                Operator = def.Operator
                TypeArguments =
                    def.TypeArguments
                        |> List.map Type.untag
                Expr = untag def.Expr
                Tag = ()
            }
        | Prim2Expr def ->
            Prim2Expr {
                Operator = def.Operator
                TypeArguments =
                    def.TypeArguments
                        |> List.map Type.untag
                Left = untag def.Left
                Right = untag def.Right
                Tag = ()
            }
        | IfExpr def ->
            IfExpr {
                Condition = untag def.Condition
                TrueBranch = untag def.TrueBranch
                FalseBranch = untag def.FalseBranch
                Tag = ()
            }
        | NumberExpr def ->
            NumberExpr {
                Number = def.Number
                Tag = ()
            }
        | IdentifierExpr def ->
            IdentifierExpr {
                Name = def.Name
                Tag = ()
            }
        | BoolExpr def ->
            BoolExpr {
                Flag = def.Flag
                Tag = ()
            }
        | ApplicationExpr def ->
            ApplicationExpr {
                Identifier = IdentifierDef.untag def.Identifier
                TypeArguments =
                    def.TypeArguments
                        |> List.map Type.untag
                Arguments =
                    def.Arguments |> List.map untag
                Tag = ()
            }
        | AnnotationExpr def ->
            AnnotationExpr {
                Expr = untag def.Expr
                Type = Type.untag def.Type
                Tag = ()
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

        let private mismatch expected actual =
            Error
                $"Expected: {Type.unparse expected}, \
                Actual: {Type.unparse actual}"

        let typeOfExpr env expr =
            result {

                let! typ =
                    match expr with
                        | LetExpr def -> typeOfLet env def
                        | Prim1Expr def -> typeOfPrim1 env def
                        | Prim2Expr def -> typeOfPrim2 env def
                        | IfExpr def -> typeOfIf env def
                        | NumberExpr def -> Ok Type.int
                        | IdentifierExpr def -> Ok (TypeBlank ())
                        | BoolExpr def -> Ok Type.bool
                        | ApplicationExpr def -> Ok (TypeBlank ())
                        | AnnotationExpr def -> Ok (TypeBlank ())

                if typ = Type.blank then
                    return! Error "Untyped expression"
                else
                    return typ
            }

        let private typeOfLet env (def : LetDef<_>) =
            typeOfExpr env def.Expr

        let private typeOfPrim1 env def =
            result {
                let! actual = typeOfExpr env def.Expr

                let check expected =
                    result {
                        if actual = expected then
                            return actual
                        else
                            return! mismatch expected actual
                    }

                match def.Operator with
                    | Add1 | Sub1 -> return! check Type.int
                    | Not -> return! check Type.bool
                    | Print | IsBool | IsNum -> return actual
            }

        let private typeOfPrim2 env def =
            result {
                let! typeLeft = typeOfExpr env def.Left
                let! typeRight = typeOfExpr env def.Right

                let check expected final =
                    result {
                        match typeLeft = expected, typeRight = expected with
                            | true, true -> return final
                            | false, _ -> return! mismatch expected typeLeft
                            | _, false -> return! mismatch expected typeRight
                    }

                match def.Operator with
                    | Plus | Minus | Times -> return! check Type.int Type.int
                    | And | Or -> return! check Type.bool Type.bool
                    | Greater | GreaterEq
                    | Less | LessEq -> return! check Type.int Type.bool
                    | Eq ->
                        if typeLeft = typeRight then
                            return Type.bool
                        else
                            return! mismatch typeLeft typeRight
            }

        let private typeOfIf env def =
            result {
                let! typeCond = typeOfExpr env def.Condition
                let! typeTrue = typeOfExpr env def.TrueBranch
                let! typeFalse = typeOfExpr env def.FalseBranch

                if typeCond = Type.bool then
                    if typeTrue = typeFalse then
                        return typeTrue
                    else
                        return! mismatch typeTrue typeFalse
                else
                    return! mismatch Type.bool typeCond
            }

    let unparse = Unparse.unparseExpr

    let typeOf = TypeCheck.typeOfExpr
