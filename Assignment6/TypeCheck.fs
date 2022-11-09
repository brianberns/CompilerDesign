namespace CompilerDesign.Assignment6

open CompilerDesign.Core

type TypeEnvironment = Map<IdentifierDef<unit>, Type<unit>>

module TypeEnvironment =

    let tryFind ident (env : TypeEnvironment) =
        match Map.tryFind ident env with
            | Some typ -> Ok typ
            | None -> Error $"Unbound identifier: {ident.Name}"

    let tryFindFunc ident env =
        match tryFind ident env with
            | Ok (TypeArrow def) -> Ok def
            | Ok _ -> Error $"Not a function: {ident.Name}"
            | Error err -> Error err

    let freeTypeVars (env : TypeEnvironment) =
        env
            |> Map.values
            |> Seq.map Type.freeTypeVars
            |> Set.unionMany

module TypeCheck =

    let private checkMissing typ =
        if typ = TypeBlank () then
            Error "Missing type"
        else Ok ()

    let private mismatch expected actual =
        Error
            $"Expected: {Type.unparse expected}, \
            Actual: {Type.unparse actual}"

    module private rec Expr =

        let typeOf (env : TypeEnvironment) expr =
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
                                    return Map.add
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
                    | Print | IsBool | IsNum -> return actual
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
                    let! argTypes =
                        def.Arguments
                            |> List.map (typeOf env)
                            |> Result.List.sequence
                    let pairs =
                        List.zip
                            typeArrowDef.InputTypes
                            argTypes
                    for (expected, actual) in pairs do
                        if expected <> actual then
                            return! Type.mismatch expected actual
                    return typeArrowDef.OutputType
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

        let typeCheck (env : TypeEnvironment) decl =
            result {
                let! arrowDef =
                    match decl.Scheme.Type with
                        | TypeArrow def ->
                            result {
                                do! Type.checkMissing def.OutputType
                                return def
                            }
                        | _ -> Error "Invalid decl scheme"

                let env' =
                    (env, decl.Parameters, arrowDef.InputTypes)
                        |||> List.fold2 (fun acc ident typ ->
                                acc |> Map.add ident typ)
                let! bodyType = Expr.typeOf env' decl.Body

                if bodyType <> arrowDef.OutputType then
                    return! Type.mismatch arrowDef.OutputType bodyType
                else
                    return (Map.add
                        decl.Identifier
                        decl.Scheme.Type
                        env : TypeEnvironment)
            }

    let typeOf program =
        result {
            let program' = Program.untag program
            let! env =
                (Map.empty, program'.Declarations)
                    ||> Result.List.foldM Decl.typeCheck
            return! Expr.typeOf env program'.Main
        }
