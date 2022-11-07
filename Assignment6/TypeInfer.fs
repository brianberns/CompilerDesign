namespace CompilerDesign.Assignment6

open CompilerDesign.Core

module TypeInfer =

    module private Type =

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

        let apply subst typ =
            (typ, subst)
                ||> List.fold (fun acc (fromIdent, toType) ->
                    substitute fromIdent toType acc)

        let rec freeTypeVars = function
            | TypeVariable ident -> Set.singleton ident
            | TypeArrow def ->
                List.fold (fun ftvs typ ->
                    Set.union (freeTypeVars typ) ftvs)
                    (freeTypeVars def.OutputType)
                    def.InputTypes
            | _ -> Set.empty

    module private Scheme =

        let substitute fromIdent toType scheme =
            if List.contains fromIdent scheme.Identifiers then
                scheme
            else {
                scheme with
                    Type =
                        Type.substitute
                            fromIdent
                            toType
                            scheme.Type
            }

        let apply subst scheme =
            (scheme, subst)
                ||> List.fold (fun acc (fromIdent, toType) ->
                    substitute fromIdent toType acc)

        let rec freeTypeVars scheme =
            Set.difference
                (Type.freeTypeVars scheme.Type)
                (set scheme.Identifiers)

    type private Substitution<'tag> =
        List<IdentifierDef<'tag> * Type<'tag>>

    module Substitution =

        let empty : Substitution<_> = List.empty

        let apply (subst : Substitution<_>) (inSubst : Substitution<_>) =
            (inSubst, subst)
                ||> List.fold (fun acc (fromIdent, toType) ->
                    List.map (fun (ident, typ) ->
                        let typ' = Type.substitute fromIdent toType typ
                        ident, typ') acc)

        let compose subst1 subst2 =
            subst1 @ apply subst1 subst2

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

    let private occurs ident typ =
        typ
            |> Type.freeTypeVars
            |> Set.contains ident

    let unify type1 type2 =

        let rec loop type1 type2 =
            result {
                match type1, type2 with

                    | TypeConstant ident1, TypeConstant ident2
                        when ident1 = ident2 ->
                        return Substitution.empty

                    | TypeVariable ident, _
                        when type2 |> occurs ident |> not ->
                        return [ ident, type2 ]

                    | _, TypeVariable ident
                        when type1 |> occurs ident |> not ->
                        return [ ident, type1 ]

                    | TypeArrow def1, TypeArrow def2
                        when def1.InputTypes.Length = def2.InputTypes.Length ->
                        let! substs =
                            let types1 = def1.InputTypes @ [def1.OutputType]
                            let types2 = def2.InputTypes @ [def2.OutputType]
                            (types1, types2)
                                ||> List.map2 loop
                                |> Result.List.sequence
                        return List.reduce Substitution.compose substs
                    | _ ->
                        return! Error "Could not unify"
            }

        loop
            (Type.untag type1)
            (Type.untag type2)
