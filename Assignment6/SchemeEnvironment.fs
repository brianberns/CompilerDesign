namespace CompilerDesign.Assignment6

open CompilerDesign.Core
open Substitution

/// Maps primitive operations and declared function to their schemes.
/// E.g. isbool : <'a>('a -> Bool)
/// E.g. myIdentityFunction : <'a>('a -> 'a)
type private SchemeEnvironment =
    Map<string, Scheme<unit>>

module SchemeEnvironment =

    let apply (subst : Substitution<_>) (env : SchemeEnvironment) =
        (env, subst)
            ||> List.fold (fun acc (fromIdent, toType) ->
                Map.map (fun _ typ ->
                    Scheme.substitute fromIdent toType typ) acc)

    let initial : SchemeEnvironment =

        let prim1s =
            [
                Add1, "(Int -> Int)"
                Sub1, "(Int -> Int)"
                Print, "<'a>('a -> 'a)"
                IsBool, "<'a>('a -> Bool)"
                IsNum, "<'a>('a -> Bool)"
                Not, "(Bool -> Bool)"
            ] |> List.map (fun (op, text) ->
                Prim1.unparse op, text)

        let prim2s =
            [
                Plus, "(Int, Int -> Int)"
                Minus, "(Int, Int -> Int)"
                Times, "(Int, Int -> Int)"
                And, "(Bool, Bool -> Bool)"
                Or, "(Bool, Bool -> Bool)"
                Greater, "(Int, Int -> Bool)"
                GreaterEq, "(Int, Int -> Bool)"
                Less, "(Int, Int -> Bool)"
                LessEq, "(Int, Int -> Bool)"
                Eq, "<'a>('a, 'a -> Bool)"
            ] |> List.map (fun (op, text) ->
                Prim2.unparse op, text)

        prim1s @ prim2s
            |> Result.List.traverse (fun (name, text) ->
                result {
                    let! scheme = Parser.Scheme.parse text
                    return name, Scheme.untag scheme
                })
            |> Result.get
            |> Map

    let tryAdd name scheme (env : SchemeEnvironment) =
        if Map.containsKey name env then
            Error $"Duplicate scheme name: {name}"
        else
            let env : SchemeEnvironment = Map.add name scheme env
            Ok env

    let private tryFind name (env : SchemeEnvironment) =
        env
            |> Map.tryFind name
            |> Option.map Result.Ok
            |> Option.defaultWith (fun () ->
                Result.Error $"Name not found: {name}")

    let tryFindIdent ident =
        tryFind ident.Name

    let tryFindPrim1 prim1 =
        tryFind (Prim1.unparse prim1)

    let tryFindPrim2 prim2 =
        tryFind (Prim2.unparse prim2)
