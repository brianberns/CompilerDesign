namespace CompilerDesign.Assignment6

type TypeEnvironment = Map<IdentifierDef<unit>, Type<unit>>

module TypeEnvironment =

    let empty : TypeEnvironment = Map.empty

    let tryAdd ident typ (env : TypeEnvironment) =
        if Map.containsKey ident env then
            Error $"Duplicate identifier: {ident.Name}"
        else
            let env : TypeEnvironment = Map.add ident typ env
            Ok env

    let tryFind ident (env : TypeEnvironment) =
        match Map.tryFind ident env with
            | Some typ -> Ok typ
            | None -> Error $"Unbound identifier: {ident.Name}"

    let tryFindFunc ident env =
        match tryFind ident env with
            | Ok (TypeArrow def) -> Ok def
            | Ok _ -> Error $"Not a function: {ident.Name}"
            | Error err -> Error err

    let freeTypeVars (env : TypeEnvironment) =
        env
            |> Map.values
            |> Seq.map Type.freeTypeVars
            |> Set.unionMany
