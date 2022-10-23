namespace CompilerDesign.Assignment3

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open type SyntaxFactory

open CompilerDesign.Core

module Compiler =

    let private numericLiteral (n : int64) =
        LiteralExpression(
            SyntaxKind.NumericLiteralExpression,
            Literal(n))

    let private compileNumber num (env : env) =
        let node =
            numericLiteral num
                :> Syntax.ExpressionSyntax
        Ok (node, env)

    let private compileIdentifier ident env =
        Env.tryFind ident env
            |> Result.map (fun node -> node, env)

    let rec private compileExpr expr env : CompilerResult<_> =
        match expr with
            | NumberExpr rcd ->
                compileNumber rcd.Number env
            | Prim1Expr rcd ->
                compilePrim1 rcd.Operator rcd.Expr env
            | IdentifierExpr rcd ->
                compileIdentifier rcd.Identifier env
            | _ -> failwith "oops"

    and private compilePrim1 op expr env =
        let kind =
            match op with
                | Add1 -> SyntaxKind.AddExpression
                | Sub1 -> SyntaxKind.SubtractExpression
        result {
            let! left, env' = compileExpr expr env
            let node =
                BinaryExpression(
                    kind,
                    left,
                    numericLiteral 1L)
            return node, env'
        }

    (*
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
    *)

    let compile assemblyName text =
        result {
            let! expr = Parser.parse text
            let! node, _ = compileExpr expr Env.empty
            do! Compiler.compile_prog assemblyName node
        }
