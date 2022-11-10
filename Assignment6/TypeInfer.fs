﻿namespace CompilerDesign.Assignment6

open CompilerDesign.Core
open Substitution

type private SchemeEnvironment =
    Map<string, Scheme<unit>>

module SchemeEnvironment =

    let apply (subst : Substitution<_>) (env : SchemeEnvironment) =
        (env, subst)
            ||> List.fold (fun acc (fromIdent, toType) ->
                Map.map (fun _ typ ->
                    Scheme.substitute fromIdent toType typ) acc)

    let initial : SchemeEnvironment =

        let prim1s =
            [
                Add1, "(Int -> Int)"
                Sub1, "(Int -> Int)"
                Print, "<'a>('a -> 'a)"
                IsBool, "<'a>('a -> Bool)"
                IsNum, "<'a>('a -> Bool)"
                Not, "(Bool -> Bool)"
            ] |> List.map (fun (op, text) ->
                Prim1.unparse op, text)

        let prim2s =
            [
                Plus, "(Int, Int -> Int)"
                Minus, "(Int, Int -> Int)"
                Times, "(Int, Int -> Int)"
                And, "(Bool, Bool -> Bool)"
                Or, "(Bool, Bool -> Bool)"
                Greater, "(Int, Int -> Bool)"
                GreaterEq, "(Int, Int -> Bool)"
                Less, "(Int, Int -> Bool)"
                LessEq, "(Int, Int -> Bool)"
                Eq, "<'a>('a, 'a -> Bool)"
            ] |> List.map (fun (op, text) ->
                Prim2.unparse op, text)

        prim1s @ prim2s
            |> Result.List.traverse (fun (name, text) ->
                result {
                    let! scheme = Parser.Scheme.parse text
                    return name, Scheme.untag scheme
                })
            |> Result.get
            |> Map

    let tryFind name (env : SchemeEnvironment) =
        env
            |> Map.tryFind name
            |> Option.map Result.Ok
            |> Option.defaultValue (Result.Error $"Name not found: {name}")

    let tryFindIdent ident =
        tryFind ident.Name

    let tryFindPrim1 prim1 =
        tryFind (Prim1.unparse prim1)

    let tryFindPrim2 prim2 =
        tryFind (Prim2.unparse prim2)

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

    module rec Expr =

        let infer funenv env = function
            | NumberExpr _ -> Ok (Substitution.empty, Type.int)
            | BoolExpr _ -> Ok (Substitution.empty, Type.bool)
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
                return Substitution.empty, typ
            }

        let private inferPrim1 funenv env (def : Prim1Def<_>) =
            result {
                let! scheme =
                    SchemeEnvironment.tryFindPrim1 def.Operator funenv
                let schemeType = Scheme.instantiate scheme
                let! argSubst, argType =
                    infer funenv env def.Expr
                let outType = generateTypeVariable "out"
                let arrowType =
                    TypeArrow {
                        InputTypes = [argType]
                        OutputType = outType
                        Tag = ()
                    }
                let! subst = unify schemeType arrowType
                return
                    argSubst ++ subst,
                    Type.apply subst outType
            }

        let private inferPrim2 funenv env (def : Prim2Def<_>) =
            result {
                let! scheme =
                    SchemeEnvironment.tryFindPrim2 def.Operator funenv
                let schemeType = Scheme.instantiate scheme
                let! leftSubst, leftType =
                    infer funenv env def.Left
                let! rightSubst, rightType =
                    infer funenv env def.Right
                let outType = generateTypeVariable "out"
                let arrowType =
                    TypeArrow {
                        InputTypes = [leftType; rightType]
                        OutputType = outType
                        Tag = ()
                    }
                let! subst = unify schemeType arrowType
                return
                    leftSubst ++ rightSubst ++ subst,
                    Type.apply subst outType
            }

        let private inferIf funenv env (def : IfDef<_>) =
            result {
                let! condSubst, condType =
                    infer funenv env def.Condition
                let! trueSubst, trueType =
                    infer funenv env def.TrueBranch
                let! falseSubst, falseType =
                    infer funenv env def.Condition
                let! boolSubst = unify condType Type.bool
                let! sameSubst = unify trueType falseType
                return
                    condSubst ++ trueSubst ++ falseSubst
                        ++ boolSubst ++ sameSubst,
                    Type.apply sameSubst trueType
            }

        let private inferLet funenv env (def : LetDef<_>) =
            result {
                let! env', bindingSubst =
                    ((env, Substitution.empty), def.Bindings)
                        ||> Result.List.foldM (fun (accEnv, accSubst) binding ->
                            result {
                                let! subst, typ =
                                    infer funenv accEnv binding.Expr
                                let! accEnv' =
                                    TypeEnvironment.tryAdd
                                        binding.Identifier
                                        typ
                                        accEnv
                                return accEnv', accSubst ++ subst
                            })
                let! bodySubst, bodyType = infer funenv env' def.Expr
                return
                    bindingSubst ++ bodySubst,
                    Type.apply bodySubst bodyType
            }

        let private inferApplication funenv env (def : ApplicationDef<_>) =
            result {
                let! scheme =
                    SchemeEnvironment.tryFindIdent def.Identifier funenv
                let schemeType = Scheme.instantiate scheme
                let! argSubsts, argTypes =
                    def.Arguments
                        |> Result.List.traverse (
                            infer funenv env)
                        |> Result.map List.unzip
                let outType = generateTypeVariable "out"
                let arrowType =
                    TypeArrow {
                        InputTypes = argTypes
                        OutputType = outType
                        Tag = ()
                    }
                let! subst = unify schemeType arrowType
                return
                    (List.reduce (++) argSubsts) ++ subst,
                    Type.apply subst outType
            }

        let private inferAnnotation funenv env (def : AnnotationDef<_>) =
            result {
                let! exprSubst, exprTyp = infer funenv env def.Expr
                let! subst = unify def.Type exprTyp
                return
                    exprSubst ++ subst,
                    Type.apply subst exprTyp
            }

        let typeOf expr =
            result {
                let expr' = Expr.untag expr
                let! _, typ =
                    infer
                        SchemeEnvironment.initial
                        Map.empty
                        expr'
                return typ
            }

    (*
    module private Decl =

        // def add(x, y) : x + y
        // def id(x) : x
        let infer funenv env (decl : Decl<_>) =
            result {
                let outType = generateTypeVariable "out"
            }

    module private DeclGroup =

        let infer funenv env group =
            Expr.infer funenv env decl.Body
    *)
