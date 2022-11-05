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

    type private Substitution =
        List<IdentifierDef<unit> * Type<unit>>

    type private TypeEnvironment =
        Map<IdentifierDef<unit>, Type<unit>>

    module TypeEnvironment =

        let apply (subst : Substitution) (env : TypeEnvironment) =
            (env, subst)
                ||> List.fold (fun acc (fromIdent, toType) ->
                    Map.map (fun _ typ ->
                        Type.substitute fromIdent toType typ) acc)

    type private SchemeEnvironment =
        Map<IdentifierDef<unit>, Scheme<unit>>
