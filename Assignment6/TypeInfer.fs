namespace CompilerDesign.Assignment6

open CompilerDesign.Core

type private TypeEnvironment =
    Map<IdentifierDef<unit>, Type<unit>>

module TypeEnvironment =

    let rec substitute (fromIdent : IdentifierDef<_>) (toType : Type<_>) = function
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

type private SchemeEnvironment =
    Map<IdentifierDef<unit>, Scheme<unit>>

module SchemeEnvironment =
    ()

module TypeInfer =
    ()
