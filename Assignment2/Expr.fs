namespace CompilerDesign.Assignment2

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open type SyntaxFactory

open CompilerDesign.Core

/// Abstract syntax for the Adder language.
type expr<'a> =
    | Number of int64 * 'a
    | Prim1 of prim1 * expr<'a> * 'a
    | Let of
        bindings : List<string * expr<'a>>
            * expr<'a>
            * 'a
    | Id of name : string * 'a

and prim1 =
    | Add1
    | Sub1

/// Functions for converting expressions into Rosyln syntax
/// nodes. (It would be nice to generate a .NET assembly
/// by emitting CIL opcodes instead, but there's currently
/// no reliable way to save such an assembly to a file.
/// See https://github.com/dotnet/runtime/issues/15704.)
module Expr =

    let private numericLiteral (n : int64) =
        LiteralExpression(
            SyntaxKind.NumericLiteralExpression,
            Literal(n))

    let private compileNumber n (env : env) =
        let node =
            numericLiteral n
                :> Syntax.ExpressionSyntax
        Ok (node, env)

    let private compileId name env =
        Env.tryFind name env
            |> Result.map (fun node -> node, env)

    let rec private compileExp exp env : CompilerResult<_> =
        match exp with
            | Number (n, _) -> compileNumber n env
            | Prim1 (op, e, _) -> compilePrim1 op e env
            | Let (bindings, e, _) -> compileLet bindings e env
            | Id (name, _) -> compileId name env

    and private compilePrim1 op exp env =
        let kind =
            match op with
                | Add1 -> SyntaxKind.AddExpression
                | Sub1 -> SyntaxKind.SubtractExpression
        result {
            let! left, env' = compileExp exp env
            let node =
                BinaryExpression(
                    kind,
                    left,
                    numericLiteral 1L)
            return node, env'
        }

    and private compileLet bindings exp env =

        let rec loop bindings (env : env) =
            match bindings with
                | (name, exp) :: tail ->
                    result {
                        let! node, env' = compileExp exp env
                        let! env'' = env' |> Env.tryAdd name node
                        return! loop tail env''
                    }
                | [] -> Ok env

        result {
            let! env' = loop bindings env
            return! compileExp exp env'
        }

    /// Corresponds roughly to `compile : pos expr -> instruction list`
    /// in the assignment.
    let compile exp : CompilerResult<_> =
        result {
            let! node, _ = compileExp exp Map.empty
            return node
        }
