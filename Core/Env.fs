namespace CompilerDesign.Core

open Microsoft.CodeAnalysis.CSharp

/// Variable environment.
type env = Map<string, Syntax.ExpressionSyntax>

module Env =

    let empty : env =
        Map.empty

    let tryAdd name node (env : env) =
        if Map.containsKey name env then
            Error $"Variable already exists: {name}"
        else
            let env : env = Map.add name node env
            Ok env

    let tryFind name (env : env) =
        match Map.tryFind name env with
            | Some node -> Ok node
            | None -> Error $"Unbound identifier: {name}"
