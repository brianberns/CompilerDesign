namespace CompilerDesign.Assignment6

open CompilerDesign.Core

module TypeCheck =

    module Type =

        let checkMissing typ =
            if typ = TypeBlank () then
                Error "Missing type"
            else Ok ()

        let mismatch expected actual =
            Error
                $"Expected: {Type.unparse expected}, \
                Actual: {Type.unparse actual}"

    module private rec Expression =

        let typeOf env expr =
            result {
                let! typ =
                    match expr with
                        | NumberExpr _ -> Ok Type.int
                        | BoolExpr _ -> Ok Type.bool
                        | LetExpr def -> typeOfLet env def
                        | Prim1Expr def -> typeOfPrim1 env def
                        | Prim2Expr def -> typeOfPrim2 env def
                        | IfExpr def -> typeOfIf env def
                        | IdentifierExpr def -> TypeEnvironment.tryFind def env
                        | ApplicationExpr def -> typeOfApplication env def
                        | AnnotationExpr def -> typeOfAnnotation env def
                do! Type.checkMissing typ
                return typ
            }

        let private typeOfLet env def =
            result {
                let! env' =
                    (env, def.Bindings)
                        ||> Result.List.foldM (fun acc binding ->
                            result {
                                do! Type.checkMissing binding.Type
                                let! typeExpr = typeOf acc binding.Expr
                                if binding.Type = typeExpr then
                                    return! TypeEnvironment.tryAdd
                                        binding.Identifier
                                        typeExpr
                                        acc
                                else
                                    return! Type.mismatch binding.Type typeExpr
                            })
                return! typeOf env' def.Expr
            }

        let private typeOfPrim1 env def =
            result {
                let! actual = typeOf env def.Expr

                let check expected =
                    result {
                        if actual = expected then
                            return actual
                        else
                            return! Type.mismatch expected actual
                    }

                match def.Operator with
                    | Add1 | Sub1 -> return! check Type.int
                    | Not -> return! check Type.bool
                    | IsBool | IsNum -> return Type.bool
                    | Print -> return actual
            }

        let private typeOfPrim2 env def =
            result {
                let! typeLeft = typeOf env def.Left
                let! typeRight = typeOf env def.Right

                let check expected final =
                    result {
                        match typeLeft = expected, typeRight = expected with
                            | true, true -> return final
                            | false, _ -> return! Type.mismatch expected typeLeft
                            | _, false -> return! Type.mismatch expected typeRight
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
                            return! Type.mismatch typeLeft typeRight
            }

        let private typeOfIf env def =
            result {
                let! typeCond = typeOf env def.Condition
                let! typeTrue = typeOf env def.TrueBranch
                let! typeFalse = typeOf env def.FalseBranch

                if typeCond = Type.bool then
                    if typeTrue = typeFalse then
                        return typeTrue
                    else
                        return! Type.mismatch typeTrue typeFalse
                else
                    return! Type.mismatch Type.bool typeCond
            }

        let private typeOfApplication env def =
            result {
                let! typeArrowDef =
                    TypeEnvironment.tryFindFunc def.Identifier env
                if typeArrowDef.InputTypes.Length = def.Arguments.Length then
                    let expected = TypeArrow typeArrowDef
                    let! argTypes =
                        def.Arguments
                            |> Result.List.traverse (typeOf env)
                    let actual =
                        TypeArrow {
                            InputTypes = argTypes
                            OutputType = typeArrowDef.OutputType
                            Tag = ()
                        }
                    let! subst =
                        Substitution.unify expected actual   // needed to type check application of a polymorphic function
                    return Substitution.Type.apply
                        subst
                        typeArrowDef.OutputType
                else
                    return! Error $"Arity mismatch: \
                        expected {typeArrowDef.InputTypes.Length}, \
                        actual {def.Arguments.Length}"
            }

        let private typeOfAnnotation env def =
            result {
                do! Type.checkMissing def.Type
                let! typeExpr = typeOf env def.Expr
                if typeExpr = def.Type then
                    return typeExpr
                else
                    return! Type.mismatch def.Type typeExpr
            }

    module private Decl =

        let typeCheck env decl =
            result {
                let! typedParms, outputType = Decl.getSignature decl
                let! env' =
                    (env, typedParms)
                        ||> Result.List.foldM (fun acc (ident, typ) ->
                            acc |> TypeEnvironment.tryAdd ident typ)
                let! bodyType = Expression.typeOf env' decl.Body

                if bodyType <> outputType then
                    return! Type.mismatch outputType bodyType
            }

    module private DeclGroup =

        let typeCheck env group =
            result {
                let! env' =
                    (env, group.Decls)
                        ||> Result.List.foldM (fun env decl ->
                            TypeEnvironment.tryAdd
                                decl.Identifier
                                decl.Scheme.Type
                                env)
                for decl in group.Decls do
                    do! Decl.typeCheck env' decl
                return env'
            }

    let typeOf program =
        result {
            let program' = Program.untag program
            let! env =
                (TypeEnvironment.empty, program'.DeclGroups)
                    ||> Result.List.foldM DeclGroup.typeCheck
            return! Expression.typeOf env program'.Main
        }

    let validate program =
        result {
            let! _type = typeOf program
            return ()
        }
