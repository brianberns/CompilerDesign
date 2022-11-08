namespace CompilerDesign.Assignment6

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

    let private parseScheme text =
        let parser = Parser.Scheme.parse .>> eof
        match runParserOnString parser () "" text with
            | Success (result, _, _) -> Scheme.untag result
            | Failure (msg, _, _) -> failwith msg

    let initial : SchemeEnvironment =
        [
            Prim1.unparse Add1, "(Int -> Int)"
            Prim1.unparse Sub1, "(Int -> Int)"
            Prim1.unparse Print, "<'a>('a -> Bool)"
        ]
            |> Seq.map (fun (name, text) ->
                IdentifierDef.create name, parseScheme text)
            |> Map

module TypeInfer =

    module Scheme =

        let private generateSymbol =
            let mutable count = 0
            fun (str : string) ->
                count <- count + 1
                $"{str}_{count}"

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

        let rec loop (funenv : SchemeEnvironment) (env : TypeEnvironment) = function
            | NumberExpr _ -> Substitution.empty, Type.int
            | BoolExpr _ -> Substitution.empty, Type.bool
            | _ -> failwith "Oops"

        loop Map.empty Map.empty expr
            |> snd
