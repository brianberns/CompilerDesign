namespace CompilerDesign.Assignment6

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

    let tryFindPrim1 prim1 (env : SchemeEnvironment) =
        env
            |> Map.tryFind (Prim1.unparse prim1)
            |> Option.map Result.Ok
            |> Option.defaultValue (Result.Error "Operator not found")

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

    let rec private inferTypeExpr funenv (env : TypeEnvironment) = function
        | NumberExpr _ -> Ok (Substitution.empty, Type.int)
        | BoolExpr _ -> Ok (Substitution.empty, Type.bool)
        | Prim1Expr def -> inferTypePrim1 funenv env def
        | _ -> Error "Oops"

    and private inferTypePrim1 funenv env (def : Prim1Def<_>) =
        result {
            let! scheme =
                SchemeEnvironment.tryFindPrim1 def.Operator funenv
            let schemeType = Scheme.instantiate scheme
            let! argSubst, argType = inferTypeExpr funenv env def.Expr
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
        }

    let inferType expr =
        result {
            let! subst, typ =
                inferTypeExpr
                    SchemeEnvironment.initial
                    Map.empty
                    expr
            return Type.apply subst typ
        }
