﻿namespace CompilerDesign.Assignment6

open CompilerDesign.Core
open Substitution

type private TypeEnvironment =
    Map<IdentifierDef<unit>, Type<unit>>

module TypeEnvironment =

    let apply (subst : Substitution<_>) (env : TypeEnvironment) =
        (env, subst)
            ||> List.fold (fun acc (fromIdent, toType) ->
                Map.map (fun _ typ ->
                    Type.substitute fromIdent toType typ) acc)

    let tryFind ident (env : TypeEnvironment) =
        env
            |> Map.tryFind ident
            |> Option.map Result.Ok
            |> Option.defaultValue (Result.Error "Variable not found")

type private SchemeEnvironment =
    Map<string, Scheme<unit>>

module SchemeEnvironment =

    open FParsec

    let apply (subst : Substitution<_>) (env : SchemeEnvironment) =
        (env, subst)
            ||> List.fold (fun acc (fromIdent, toType) ->
                Map.map (fun _ typ ->
                    Scheme.substitute fromIdent toType typ) acc)

    let parseScheme text =
        let parser = Parser.Scheme.parse .>> eof
        match runParserOnString parser () "" text with
            | Success (result, _, _) -> Result.Ok result
            | Failure (msg, _, _) -> Result.Error msg

    let initial : SchemeEnvironment =
        [
            Prim1.unparse Add1, "(Int -> Int)"
            Prim1.unparse Sub1, "(Int -> Int)"
            Prim1.unparse Print, "<'a>('a -> 'a)"
            Prim1.unparse IsBool, "<'a>('a -> Bool)"
            Prim1.unparse IsNum, "<'a>('a -> Bool)"
            Prim1.unparse Not, "(Bool -> Bool)"

            Prim2.unparse Plus, "(Int, Int -> Int)"
            Prim2.unparse Minus, "(Int, Int -> Int)"
            Prim2.unparse Times, "(Int, Int -> Int)"
            Prim2.unparse And, "(Bool, Bool -> Bool)"
            Prim2.unparse Or, "(Bool, Bool -> Bool)"
            Prim2.unparse Greater, "(Int, Int -> Bool)"
            Prim2.unparse GreaterEq, "(Int, Int -> Bool)"
            Prim2.unparse Less, "(Int, Int -> Bool)"
            Prim2.unparse LessEq, "(Int, Int -> Bool)"
            Prim2.unparse Eq, "<'a>('a, 'a -> Bool)"
        ]
            |> List.map (fun (name, text) ->
                result {
                    let! scheme = parseScheme text
                    return name, Scheme.untag scheme
                })
            |> Result.List.sequence
            |> Result.get
            |> Map

    let tryFind name (env : SchemeEnvironment) =
        env
            |> Map.tryFind name
            |> Option.map Result.Ok
            |> Option.defaultValue (Result.Error $"Name not found: {name}")

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

    module Scheme =

        let instantiate scheme =
            let subst =
                scheme.TypeVariableIdents
                    |> List.map (fun ident ->
                        let tv =
                            TypeVariable {
                                Name = generateSymbol ident.Name
                                Tag = scheme.Tag
                            }
                        ident, tv)
            Type.apply subst scheme.Type

    let (++) = Substitution.compose

    let rec private inferTypeExpr funenv env = function
        | NumberExpr _ -> Ok (Substitution.empty, Type.int)
        | BoolExpr _ -> Ok (Substitution.empty, Type.bool)
        | IdentifierExpr ident -> inferIdentifier funenv env ident
        | Prim1Expr def -> inferPrim1 funenv env def
        | Prim2Expr def -> inferPrim2 funenv env def
        | IfExpr def -> inferIf funenv env def
        | _ -> Error "Oops"

    and private inferIdentifier _funenv env ident =
        result {
            let! typ = TypeEnvironment.tryFind ident env
            return Substitution.empty, typ
        }

    and private inferPrim1 funenv env (def : Prim1Def<_>) =
        result {
            let! scheme =
                SchemeEnvironment.tryFindPrim1 def.Operator funenv
            let schemeType = Scheme.instantiate scheme
            let! argSubst, argType =
                inferTypeExpr funenv env def.Expr
            let outType =
                generateSymbol "out"
                    |> IdentifierDef.create
                    |> TypeVariable
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

    and private inferPrim2 funenv env (def : Prim2Def<_>) =
        result {
            let! scheme =
                SchemeEnvironment.tryFindPrim2 def.Operator funenv
            let schemeType = Scheme.instantiate scheme
            let! leftSubst, leftType =
                inferTypeExpr funenv env def.Left
            let! rightSubst, rightType =
                inferTypeExpr funenv env def.Right
            let outType =
                generateSymbol "out"
                    |> IdentifierDef.create
                    |> TypeVariable
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

    and private inferIf funenv env (def : IfDef<_>) =
        result {
            let! condSubst, condType =
                inferTypeExpr funenv env def.Condition
            let! trueSubst, trueType =
                inferTypeExpr funenv env def.TrueBranch
            let! falseSubst, falseType =
                inferTypeExpr funenv env def.Condition
            let triSubst = condSubst ++ trueSubst ++ falseSubst
            return failwith "moo"
        }

    let inferType expr =
        result {
            let expr' = Expr.untag expr
            let! _, typ =
                inferTypeExpr
                    SchemeEnvironment.initial
                    Map.empty
                    expr'
            return typ
        }
