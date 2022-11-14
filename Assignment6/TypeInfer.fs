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

    module Scheme =

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

    module private rec Expr =

        let annotate expr typ =
            AnnotationExpr {
                Expr = expr
                Type = typ
                Tag = expr.Tag'
            }

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
                let fullType = Type.apply subst outType
                let expr =
                    annotate
                        (Prim1Expr { def with Expr = argExpr })
                        fullType
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
                let fullType = Type.apply subst outType
                let expr =
                    annotate
                        (Prim2Expr {
                            def with
                                Left = leftExpr
                                Right = rightExpr })
                        fullType
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
                    infer funenv env def.Condition
                let! boolSubst = unify condType Type.bool
                let! sameSubst = unify trueType falseType
                let fullType = Type.apply sameSubst trueType
                let expr =
                    annotate
                        (IfExpr {
                            def with
                                Condition = condExpr
                                TrueBranch = trueExpr
                                FalseBranch = falseExpr })
                        fullType
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
                let fullType = Type.apply bodySubst bodyType
                let expr =
                    annotate
                        (LetExpr {
                            def with
                                Bindings = List.rev bindingsRev
                                Expr = bodyExpr })
                        fullType
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
                let fullType = Type.apply subst outType
                let expr =
                    annotate
                        (ApplicationExpr {
                            def with Arguments = argExprs })
                        fullType
                return
                    (List.reduce (++) argSubsts) ++ subst,
                    fullType,
                    expr
            }

        let private inferAnnotation funenv env (def : AnnotationDef<_>) =
            result {
                let! innerSubst, innerType, innerExpr =
                    infer funenv env def.Expr
                let! annotSubst = unify innerType def.Type
                let fullType = Type.apply annotSubst innerType
                let expr =
                    annotate
                        (AnnotationExpr {
                            def with Expr = innerExpr })
                        fullType
                return innerSubst ++ annotSubst,
                    fullType,
                    expr
            }

    module private Decl =

        let infer funenv env (decl : Decl<_>) =
            result {

                let! typedParms, _ = Decl.getSignature decl   // to-do: what if declared output type is more restrictive than inferred body type?
                let! env' =
                    (env, typedParms)
                        ||> Result.List.foldM (fun acc (ident, typ) ->
                            let typ' =
                                if typ = TypeBlank () then
                                    generateTypeVariable ident.Name
                                else typ
                            acc |> TypeEnvironment.tryAdd ident typ')

                let! bodySubst, bodyType, bodyExpr =
                    Expr.infer funenv env' decl.Body

                let! parmTypes =
                    decl.Parameters
                        |> Result.List.traverse (fun ident ->
                            result {
                                let! typ =
                                    env' |> TypeEnvironment.tryFind ident
                                return Type.apply bodySubst typ
                            })

                let scheme =
                    TypeArrow {
                        InputTypes = parmTypes
                        OutputType = bodyType
                        Tag = ()
                    } |> Scheme.generalize env
                let! funenv' =
                    funenv
                        |> SchemeEnvironment.tryAdd
                            decl.Identifier.Name
                            scheme

                let decl' =
                    {
                        decl with
                            Body = bodyExpr
                            Scheme = scheme
                    }

                return bodySubst, funenv', decl'
            }

    module private DeclGroup =

        let infer funenv env group =
            result {
                let! subst, typ, declsRev =
                    ((Substitution.empty, funenv, []), group.Decls)
                        ||> Result.List.foldM (fun (subst, accFunenv, accDecls) decl ->
                            result {
                                let! subst', accFunenv', decl' =
                                    Decl.infer accFunenv env decl
                                return
                                    subst ++ subst',
                                    accFunenv',
                                    decl' :: accDecls
                            })
                let group' =
                    { group with Decls = List.rev declsRev }
                return subst, typ, group'
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
                    Expr.infer 
                        funenv
                        TypeEnvironment.empty
                        program.Main
                let fullType =
                    Type.apply substDecls mainType
                let program' =
                    {
                        program with
                            DeclGroups = List.rev groupsRev
                            Main = Expr.annotate mainExpr fullType
                    }
                return
                    substDecls ++ mainSubst,
                    fullType,
                    program'
            }

    let typeOf program =
        result {
            let! _, typ, _ =
                program
                    |> Program.untag
                    |> Program.infer
            return typ
        }

    let annotate program =
        result {
            let! _, _, program' =
                program
                    |> Program.untag
                    |> Program.infer
            return program'
        }
