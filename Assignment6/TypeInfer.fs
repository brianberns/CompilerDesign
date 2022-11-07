namespace CompilerDesign.Assignment6

open CompilerDesign.Core

module TypeInfer =

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

    module Scheme =

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

    type Substitution<'tag> =
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

        let err = Error "Could not unify"

        let rec loop type1 type2 =
            result {
                match type1, type2 with

                    | TypeBlank (), _
                    | _, TypeBlank () -> return! err

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
                        let pairs =
                            let types1 = def1.InputTypes @ [def1.OutputType]
                            let types2 = def2.InputTypes @ [def2.OutputType]
                            List.zip types1 types2
                        return! (Substitution.empty, pairs)
                            ||> Result.List.foldM (fun subst (t1, t2) ->
                                result {
                                    let! subst' =
                                        loop
                                            (Type.apply subst t1)
                                            (Type.apply subst t2)
                                    return Substitution.compose subst subst'
                                })

                    | _ -> return! err
            }

        loop
            (Type.untag type1)
            (Type.untag type2)
