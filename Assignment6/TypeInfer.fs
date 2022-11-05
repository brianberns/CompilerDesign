namespace CompilerDesign.Assignment6

open CompilerDesign.Core

type private TypeEnvironment =
    Map<IdentifierDef<unit>, Type<unit>>

type private SchemeEnvironment =
    Map<IdentifierDef<unit>, Scheme<unit>>

type private Substitution =
    List<IdentifierDef<unit> * Type<unit>>

module TypeInfer =

    module private Type =

        let rec substitute fromIdent toType = function
            | TypeVariable ident as tv ->
                if ident = fromIdent then toType
                else tv
            | TypeArrow def ->
                TypeArrow {
                    InputTypes =
                        def.InputTypes
                            |> List.map (substitute fromIdent toType)
                    OutputType =
                        def.OutputType
                            |> substitute fromIdent toType
                    Tag = def.Tag
                }
            | typ -> typ

        let rec apply subst typ =
            (typ, subst)
                ||> List.fold (fun acc (fromIdent, toType) ->
                    substitute fromIdent toType acc)

    module private Scheme =

        let rec substitute fromIdent toType scheme =
            if scheme.Identifiers |> List.contains fromIdent then
                scheme
            else
                {
                    scheme with
                        Type =
                            scheme.Type
                                |> Type.substitute
                                    fromIdent
                                    toType
                }
