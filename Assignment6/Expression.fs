namespace CompilerDesign.Assignment6

/// Primitive unary operators.
type Prim1 =
    | Add1
    | Sub1
    | Print
    | IsBool
    | IsNum
    | Not

module Prim1 =

    let unparse = function
        | Not -> "!"
        | prim1 -> (string prim1).ToLower()

/// Primitive binary operators.
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

module Prim2 =

    let unparse = function
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

/// Numeric literal.
type NumberDef<'tag> =
    {
        Number : int
        Tag : 'tag
    }

/// Boolean literal.
type BoolDef<'tag> =
    {
        Flag : bool
        Tag : 'tag
    }

/// An expression that evaluates to a value.
type Expression<'tag> =

    /// E.g. "let x = 0 in 2 * x".
    | LetExpr of LetDef<'tag>

    /// E.g. "add1(0)".
    | Prim1Expr of Prim1Def<'tag>

    /// E.g. "1 + 2".
    | Prim2Expr of Prim2Def<'tag>

    /// E.g. "if flag: 1 else: 2".
    | IfExpr of IfDef<'tag>

    /// Numeric literal. E.g. "1".
    | NumberExpr of NumberDef<'tag>

    /// Value identifier. E.g. "x".
    | IdentifierExpr of IdentifierDef<'tag>

    /// Boolean literal. E.g. "true".
    | BoolExpr of BoolDef<'tag>

    /// Function application. E.g. "f(3)".
    | ApplicationExpr of ApplicationDef<'tag>

    /// Type annotation. E.g. "(x : Int)".
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
        Expr : Expression<'tag>
        Tag : 'tag
    }

and Binding<'tag> =
    {
        Identifier : IdentifierDef<'tag>
        Type : Type<'tag>
        Expr : Expression<'tag>
    }

and Prim1Def<'tag> =
    {
        Operator : Prim1
        TypeArguments : List<Type<'tag>>
        Expr : Expression<'tag>
        Tag : 'tag
    }

and Prim2Def<'tag> =
    {
        Operator : Prim2
        TypeArguments : List<Type<'tag>>
        Left : Expression<'tag>
        Right : Expression<'tag>
        Tag : 'tag
    }

and IfDef<'tag> =
    {
        Condition : Expression<'tag>
        TrueBranch : Expression<'tag>
        FalseBranch : Expression<'tag>
        Tag : 'tag
    }

and ApplicationDef<'tag> =
    {
        /// Name of function being called.
        Identifier : IdentifierDef<'tag>
        TypeArguments : List<Type<'tag>>
        Arguments : List<Expression<'tag>>
        Tag : 'tag
    }

and AnnotationDef<'tag> =
    {
        Expr : Expression<'tag>
        Type: Type<'tag>
        Tag : 'tag
    }

module Expression =

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
            IdentifierExpr (
                IdentifierDef.create def.Name)
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
                let op = Prim1.unparse def.Operator
                let typeArgs = unparseTypeArgs def.TypeArguments
                let expr = unparseExpr def.Expr
                $"{op}{typeArgs}({expr})"
            | Prim2Expr def ->
                let left = unparseExpr def.Left
                let op = Prim2.unparse def.Operator
                let typeArgs = unparseTypeArgs def.TypeArguments
                let right = unparseExpr def.Right
                $"({left} {op}{typeArgs} {right})"
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

    let unparse = Unparse.unparseExpr
