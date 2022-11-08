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

type private SchemeEnvironment =
    Map<IdentifierDef<unit>, Scheme<unit>>

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
                    let ident = IdentifierDef.create name
                    let! scheme = parseScheme text
                    return ident, Scheme.untag scheme
                })
            |> Result.List.sequence
            |> Result.get
            |> Map

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

    let inferType expr =

        let rec loop (funenv : SchemeEnvironment) (env : TypeEnvironment) expr =
            result {
                match expr with

                    | NumberExpr _ ->
                        return Substitution.empty, Type.int

                    | BoolExpr _ ->
                        return Substitution.empty, Type.bool

                    | Prim1Expr def ->
                        let scheme = funenv[Prim1.unparse def.Operator |> IdentifierDef.create]   // to-do: too ugly
                        let schemeType = Scheme.instantiate scheme
                        let! argSubst, argType = loop funenv env def.Expr
                        let retType =
                            generateSymbol "ret"
                                |> IdentifierDef.create
                                |> TypeVariable
                        let arrowType =
                            TypeArrow {
                                InputTypes = [argType]
                                OutputType = retType
                                Tag = ()
                            }
                        let! subst = unify schemeType arrowType
                        return subst, retType

                    | _ -> return! Error "Oops"
            }

        result {
            let! subst, typ =
                loop
                    SchemeEnvironment.initial
                    Map.empty
                    expr
            return Type.apply subst typ
        }
