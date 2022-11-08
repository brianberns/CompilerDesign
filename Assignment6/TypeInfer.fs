﻿namespace CompilerDesign.Assignment6

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

    let apply (subst : Substitution<_>) (env : SchemeEnvironment) =
        (env, subst)
            ||> List.fold (fun acc (fromIdent, toType) ->
                Map.map (fun _ typ ->
                    Scheme.substitute fromIdent toType typ) acc)

module TypeInfer =

    module Scheme =

        let private generateSymbol =
            let mutable count = 0
            fun (str : string) ->
                count <- count + 1
                $"{str}_{count}"

        let instantiate scheme =
            let subst =
                scheme.Identifiers
                    |> List.map (fun ident ->
                        let tv =
                            let sym = generateSymbol ident.Name
                            TypeVariable { Name = sym; Tag = () }
                        ident, tv)
            Type.apply subst scheme.Type

    let inferType expr =

        let rec loop (funenv : SchemeEnvironment) (env : TypeEnvironment) = function
            | NumberExpr _ -> Substitution.empty, Type.int
            | BoolExpr _ -> Substitution.empty, Type.bool
            | _ -> failwith "Oops"

        loop Map.empty Map.empty expr
            |> snd
