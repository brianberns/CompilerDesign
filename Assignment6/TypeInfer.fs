﻿namespace CompilerDesign.Assignment6

open CompilerDesign.Core
open Substitution

module TypeInfer =

    let private generateSymbol =
        let mutable count = 0
        fun (str : string) ->
            count <- count + 1
            $"{str}_{count}"

    let private generateTypeVariable str =
        generateSymbol str
            |> IdentifierDef.create
            |> TypeVariable

    module private Scheme =

        /// Prepares a scheme for instantiation, but does not infer
        /// anything about its type.
        /// E.g. <'a>('a -> _ -> _) => <'a>('a -> 'b -> 'c)
        let preinstantiate scheme =

            let rec replaceBlanks = function
                | TypeBlank _ -> generateTypeVariable "tv"
                | TypeArrow def ->
                    TypeArrow {
                        def with
                            InputTypes =
                                List.map replaceBlanks def.InputTypes
                            OutputType = replaceBlanks def.OutputType
                    }
                | typ -> typ

            let typ = replaceBlanks scheme.Type
            { scheme with Type = typ }

        let instantiate scheme =
            let subst =
                scheme.TypeVariableIdents
                    |> List.map (fun ident ->
                        let typ = generateTypeVariable ident.Name
                        ident, typ)
            Type.apply subst scheme.Type

        let generalize env typ =
            let tvIdents =
                let typFtvs = Type.freeTypeVars typ
                let envFtvs = TypeEnvironment.freeTypeVars env
                List.ofSeq (typFtvs - envFtvs)
            {
                TypeVariableIdents = tvIdents
                Type = typ
                Tag = ()
            }

    let (++) = Substitution.compose

    module private rec Expression =

        let annotate subst typ expr =
            let fullType = Type.apply subst typ
            let def =
                match Expression.apply subst expr with
                    | AnnotationExpr def when def.Type = fullType ->
                        { def with Tag = expr.Tag' }
                    | expr' ->
                        {
                            Expr = expr'
                            Type = fullType
                            Tag = expr.Tag'
                        }
            fullType, AnnotationExpr def

        let infer funenv env expr =
            match expr with
                | NumberExpr _ -> Ok (Substitution.empty, Type.int, expr)
                | BoolExpr _ -> Ok (Substitution.empty, Type.bool, expr)
                | IdentifierExpr ident -> inferIdentifier funenv env ident
                | Prim1Expr def -> inferPrim1 funenv env def
                | Prim2Expr def -> inferPrim2 funenv env def
                | IfExpr def -> inferIf funenv env def
                | LetExpr def -> inferLet funenv env def
                | ApplicationExpr def -> inferApplication funenv env def
                | AnnotationExpr def -> inferAnnotation funenv env def

        let private inferIdentifier _funenv env ident =
            result {
                let! typ = TypeEnvironment.tryFind ident env
                let expr = IdentifierExpr ident   // don't annotate
                return Substitution.empty, typ, expr
            }

        let private inferPrim1 funenv env (def : Prim1Def<_>) =
            result {
                let! scheme =
                    SchemeEnvironment.tryFindPrim1 def.Operator funenv
                let schemeType = Scheme.instantiate scheme
                let! argSubst, argType, argExpr =
                    infer funenv env def.Expr
                let outType = generateTypeVariable "out"
                let arrowType =
                    TypeArrow {
                        InputTypes = [argType]
                        OutputType = outType
                        Tag = ()
                    }
                let! subst = unify schemeType arrowType
                let fullType, expr =
                    annotate
                        subst
                        outType
                        (Prim1Expr { def with Expr = argExpr })
                return argSubst ++ subst, fullType, expr
            }

        let private inferPrim2 funenv env (def : Prim2Def<_>) =
            result {
                let! scheme =
                    SchemeEnvironment.tryFindPrim2 def.Operator funenv
                let schemeType = Scheme.instantiate scheme
                let! leftSubst, leftType, leftExpr =
                    infer funenv env def.Left
                let! rightSubst, rightType, rightExpr =
                    infer funenv env def.Right
                let outType = generateTypeVariable "out"
                let arrowType =
                    TypeArrow {
                        InputTypes = [leftType; rightType]
                        OutputType = outType
                        Tag = ()
                    }
                let! subst = unify schemeType arrowType
                let fullType, expr =
                    annotate
                        subst
                        outType
                        (Prim2Expr {
                            def with
                                Left = leftExpr
                                Right = rightExpr })
                return
                    leftSubst ++ rightSubst ++ subst,
                    fullType,
                    expr
            }

        let private inferIf funenv env (def : IfDef<_>) =
            result {
                let! condSubst, condType, condExpr =
                    infer funenv env def.Condition
                let! trueSubst, trueType, trueExpr =
                    infer funenv env def.TrueBranch
                let! falseSubst, falseType, falseExpr =
                    infer funenv env def.FalseBranch
                let! boolSubst = unify condType Type.bool
                let! sameSubst = unify trueType falseType
                let fullType, expr =
                    annotate
                        sameSubst
                        trueType
                        (IfExpr {
                            def with
                                Condition = condExpr
                                TrueBranch = trueExpr
                                FalseBranch = falseExpr })
                return
                    condSubst ++ trueSubst ++ falseSubst
                        ++ boolSubst ++ sameSubst,
                    fullType,
                    expr
            }

        let private inferBinding funenv env (binding : Binding<_>) =
            result {
                let! subst, typ, expr =
                    infer funenv env binding.Expr
                let! env' =
                    TypeEnvironment.tryAdd
                        binding.Identifier
                        typ
                        env
                let binding' =
                    {
                        binding with
                            Expr = expr
                            Type = typ
                    }
                return env', subst, binding'
            }

        let private inferLet funenv env (def : LetDef<_>) =
            result {
                let! env', bindingSubst, bindingsRev =
                    ((env, Substitution.empty, []), def.Bindings)
                        ||> Result.List.foldM (fun (accEnv, accSubst, accBindings) binding ->
                            result {
                                let! accEnv', subst, binding' =
                                    inferBinding funenv accEnv binding
                                return
                                    accEnv',
                                    accSubst ++ subst,
                                    binding' :: accBindings
                            })
                let! bodySubst, bodyType, bodyExpr =
                    infer funenv env' def.Expr
                let fullType, expr =
                    annotate
                        bodySubst
                        bodyType
                        (LetExpr {
                            def with
                                Bindings = List.rev bindingsRev
                                Expr = bodyExpr })
                return
                    bindingSubst ++ bodySubst,
                    fullType,
                    expr
            }

        let private inferApplication funenv env (def : ApplicationDef<_>) =
            result {
                let! scheme =
                    SchemeEnvironment.tryFindIdent def.Identifier funenv
                let schemeType = Scheme.instantiate scheme
                let! argSubsts, argTypes, argExprs =
                    def.Arguments
                        |> Result.List.traverse (
                            infer funenv env)
                        |> Result.map List.unzip3
                let outType = generateTypeVariable "out"
                let arrowType =
                    TypeArrow {
                        InputTypes = argTypes
                        OutputType = outType
                        Tag = ()
                    }
                let! subst = unify schemeType arrowType
                let fullType, expr =
                    annotate
                        subst
                        outType
                        (ApplicationExpr {
                            def with Arguments = argExprs })
                let argSubst =
                    argSubsts
                        |> List.fold (++) Substitution.empty
                return
                    argSubst ++ subst,
                    fullType,
                    expr
            }

        let private inferAnnotation funenv env (def : AnnotationDef<_>) =
            result {
                let! innerSubst, innerType, innerExpr =
                    infer funenv env def.Expr
                if def.Type = TypeBlank () then
                    return innerSubst, innerType, innerExpr
                else
                    let! annotSubst = unify innerType def.Type
                    let fullType, expr =
                        annotate
                            annotSubst
                            innerType
                            (AnnotationExpr {
                                def with Expr = innerExpr })
                    return innerSubst ++ annotSubst,
                        fullType,
                        expr
            }

    module private Decl =

        /// Fleshes out a function's scheme and adds it to the given
        /// environment. This is different from instantiation.
        let preinstantiate funenv decl =
            result {
                let scheme = Scheme.preinstantiate decl.Scheme
                let! funenv' =
                    funenv
                        |> SchemeEnvironment.tryAdd
                            decl.Identifier.Name
                            scheme
                return funenv', { decl with Scheme = scheme }
            }

        let infer funenv env (decl : Decl<_>) =
            result {

                    // make the function's parameters available
                let! typedParms, outputType = Decl.getSignature decl
                let! env' =
                    (env, typedParms)
                        ||> Result.List.foldM (fun acc (ident, typ) ->
                            acc |> TypeEnvironment.tryAdd ident typ)

                    // infer the body's type
                let! bodySubst, bodyType, bodyExpr =
                    Expression.infer funenv env' decl.Body

                    // unify inferred output type
                let! outputSubst = unify outputType bodyType

                let subst = bodySubst ++ outputSubst
                let funenv' = SchemeEnvironment.apply subst funenv
                let decl' = { decl with Body = bodyExpr }
                return subst, funenv', decl'
            }

        let generalize funenv env subst decl =
            result {
                let scheme =
                    Type.apply subst decl.Scheme.Type
                        |> Scheme.generalize env
                let! funenv' =
                    funenv
                        |> SchemeEnvironment.tryAdd
                            decl.Identifier.Name
                            scheme
                let decl' = { decl with Scheme = scheme }
                return funenv', decl'
            }

    module private DeclGroup =

        let infer funenv env group =
            result {

                let! funenv', declsRev =
                    ((funenv, []), group.Decls)
                        ||> Result.List.foldM (fun (accFunenv, accDecls) decl ->
                            result {
                                let! accFunenv', decl' =
                                    Decl.preinstantiate accFunenv decl
                                return
                                    accFunenv',
                                    decl' :: accDecls
                            })

                let! subst, _funenv, declsRev' =
                    ((Substitution.empty, funenv', []), List.rev declsRev)
                        ||> Result.List.foldM (fun (accSubst, accFunenv, accDecls) decl ->
                            result {
                                let! declSubst, accFunenv', decl' =
                                    Decl.infer accFunenv env decl
                                return
                                    accSubst ++ declSubst,
                                    accFunenv',
                                    decl' :: accDecls
                            })

                let! funenv'', declsRev'' =
                    ((funenv, []), List.rev declsRev')
                        ||> Result.List.foldM (fun (accFunenv, accDecls) decl ->
                            result {
                                let! accFunenv', decl' =
                                    Decl.generalize accFunenv env subst decl
                                return
                                    accFunenv',
                                    decl' :: accDecls
                            })

                let group' =
                    { group with Decls = List.rev declsRev'' }
                return subst, funenv'', group'
            }

    module private Program =

        let infer program =
            result {
                let! substDecls, funenv, groupsRev =
                    ((Substitution.empty, SchemeEnvironment.initial, []), program.DeclGroups)
                        ||> Result.List.foldM (fun (subst, accFunenv, accGroups) group ->
                            result {
                                let! subst', accFunenv', group' =
                                    DeclGroup.infer
                                        accFunenv
                                        TypeEnvironment.empty
                                        group
                                return
                                    subst ++ subst',
                                    accFunenv',
                                    group' :: accGroups
                            })
                let! mainSubst, mainType, mainExpr =
                    Expression.infer 
                        funenv
                        TypeEnvironment.empty
                        program.Main
                let fullType, main =
                    Expression.annotate
                        substDecls
                        mainType
                        mainExpr
                let program' =
                    {
                        program with
                            DeclGroups = List.rev groupsRev
                            Main = main
                    }
                return
                    substDecls ++ mainSubst,
                    fullType,
                    program'
            }

    /// Infers the type of the given program's main expression.
    let typeOf program =
        result {
            let! _, typ, _ =
                program
                    |> Program.untag
                    |> Program.infer
            return typ
        }

    /// Answers a fully annotated version of the given program.
    let annotate program =
        result {
            let! _, _, program' =
                program
                    |> Program.untag
                    |> Program.infer
            return program'
        }
