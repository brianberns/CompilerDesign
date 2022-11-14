namespace CompilerDesign.Assignment6

open CompilerDesign.Core

type Substitution<'tag> =
    List<IdentifierDef<'tag> * Type<'tag>>

module Substitution =

    module Type =

        let substitute fromIdent toType inType =

            let rec loop = function
                | TypeVariable ident as tv ->
                    if ident = fromIdent then toType
                    else tv
                | TypeArrow def ->
                    TypeArrow {
                        InputTypes = List.map loop def.InputTypes
                        OutputType = loop def.OutputType
                        Tag = def.Tag
                    }
                | typ -> typ

            loop inType

        let apply (subst : Substitution<_>) typ =
            (typ, subst)
                ||> List.fold (fun acc (fromIdent, toType) ->
                    substitute fromIdent toType acc)

    let empty : Substitution<_> = List.empty

    let apply (subst : Substitution<_>) (inSubst : Substitution<_>) =
        (inSubst, subst)
            ||> List.fold (fun acc (fromIdent, toType) ->
                List.map (fun (ident, typ) ->
                    let typ' = Type.substitute fromIdent toType typ
                    ident, typ') acc)

    let compose (subst1 : Substitution<_>) subst2 : Substitution<_> =
        subst1 @ apply subst1 subst2

    let private occurs ident typ =
        typ
            |> Type.freeTypeVars
            |> Set.contains ident

    let unify type1 type2 =

        let rec loop type1 type2 =
            let err =
                Error $"Could not unify {Type.unparse type1} \
                    and {Type.unparse type2}"
            result {
                match type1, type2 with

                    | TypeBlank (), _
                    | _, TypeBlank () -> return! err

                    | TypeConstant ident1, TypeConstant ident2
                        when ident1 = ident2 ->
                        return empty

                    | TypeVariable ident1, TypeVariable ident2   // avoid occurs check
                        when ident1 = ident2 ->
                        return empty

                    | TypeVariable ident, _
                        when type2 |> occurs ident |> not ->
                        return [ ident, type2 ]

                    | _, TypeVariable ident
                        when type1 |> occurs ident |> not ->
                        return [ ident, type1 ]

                    | TypeArrow def1, TypeArrow def2
                        when def1.InputTypes.Length = def2.InputTypes.Length ->
                        let pairs =
                            let types1 = def1.InputTypes @ [def1.OutputType]
                            let types2 = def2.InputTypes @ [def2.OutputType]
                            List.zip types1 types2
                        return! (empty, pairs)
                            ||> Result.List.foldM (fun subst (t1, t2) ->
                                result {
                                    let! subst' =
                                        loop
                                            (Type.apply subst t1)
                                            (Type.apply subst t2)
                                    return compose subst subst'
                                })

                    | _ -> return! err
            }

        loop
            (Type.untag type1)
            (Type.untag type2)

    module Scheme =

        let substitute fromIdent toType scheme =
            if List.contains fromIdent scheme.TypeVariableIdents then
                scheme
            else {
                scheme with
                    Type =
                        Type.substitute
                            fromIdent
                            toType
                            scheme.Type
            }

        let apply (subst : Substitution<_>) scheme =
            (scheme, subst)
                ||> List.fold (fun acc (fromIdent, toType) ->
                    substitute fromIdent toType acc)

        let rec freeTypeVars scheme =
            Set.difference
                (Type.freeTypeVars scheme.Type)
                (set scheme.TypeVariableIdents)

    module TypeEnvironment =

        let apply (subst : Substitution<_>) (env : TypeEnvironment) =
            (env, subst)
                ||> List.fold (fun acc (fromIdent, toType) ->
                    Map.map (fun _ typ ->
                        Type.substitute fromIdent toType typ) acc)

    module Expr =

        let rec apply (subst : Substitution<_>) = function
            | LetExpr def -> applyLet subst def
            | Prim1Expr def -> applyPrim1 subst def
            | Prim2Expr def -> applyPrim2 subst def
            | IfExpr def -> applyIf subst def
            | ApplicationExpr def -> applyApplication subst def
            | AnnotationExpr def -> applyAnnotation subst def
            | expr -> expr

        and private applyBinding subst (binding : Binding<_>) =
            { binding with
                Type = Type.apply subst binding.Type }

        and private applyLet subst def =
            let bindings =
                def.Bindings
                    |> List.map (applyBinding subst)
            LetExpr {
                def with
                    Bindings = bindings
                    Expr = apply subst def.Expr }

        and private applyPrim1 subst def =
            let typeArgs =
                def.TypeArguments
                    |> List.map (Type.apply subst)
            Prim1Expr {
                def with
                    TypeArguments = typeArgs
                    Expr = apply subst def.Expr }

        and private applyPrim2 subst def =
            let typeArgs =
                def.TypeArguments
                    |> List.map (Type.apply subst)
            Prim2Expr {
                def with
                    TypeArguments = typeArgs
                    Left = apply subst def.Left
                    Right = apply subst def.Right
            }

        and private applyIf subst def =
            IfExpr {
                def with
                    Condition = apply subst def.Condition
                    TrueBranch = apply subst def.TrueBranch
                    FalseBranch = apply subst def.FalseBranch
            }

        and private applyApplication subst def =
            let typeArgs =
                def.TypeArguments
                    |> List.map (Type.apply subst)
            let args =
                def.Arguments
                    |> List.map (apply subst)
            ApplicationExpr {
                def with
                    TypeArguments = typeArgs
                    Arguments = args
            }

        and private applyAnnotation subst def =
            AnnotationExpr {
                def with
                    Expr = apply subst def.Expr
                    Type = Type.apply subst def.Type
            }
