namespace CompilerDesign.Assignment3

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open type SyntaxFactory

open CompilerDesign.Core

module Compiler =

    let private numericLiteral (n : int) =
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
            | LetExpr rcd -> failwith "oops"
            | Prim1Expr rcd ->
                compilePrim1 rcd.Operator rcd.Expr env
            | Prim2Expr rcd ->
                compilePrim2 rcd.Operator rcd.Left rcd.Right env
            | IfExpr rcd ->
                compileIf rcd.Condition rcd.TrueBranch rcd.FalseBranch env
            | NumberExpr rcd ->
                compileNumber rcd.Number env
            | IdentifierExpr rcd ->
                compileIdentifier rcd.Identifier env

    and private compilePrim1 op expr env =
        let kind =
            match op with
                | Add1 -> SyntaxKind.AddExpression
                | Sub1 -> SyntaxKind.SubtractExpression
        result {
            let! left, _ = compileExpr expr env
            let node =
                BinaryExpression(
                    kind,
                    left,
                    numericLiteral 1)
            return node, env
        }

    and private compilePrim2 op left right env =
        let kind =
            match op with
                | Plus -> SyntaxKind.AddExpression
                | Minus -> SyntaxKind.SubtractExpression
                | Times -> SyntaxKind.MultiplyExpression
        result {
            let! leftNode, _ = compileExpr left env
            let! rightNode, _ = compileExpr right env
            let node =
                BinaryExpression(
                    kind,
                    leftNode,
                    rightNode)
            return node, env
        }

    and private compileIf cond trueBranch falseBranch env =
        result {
            let! condNode, _ = compileExpr cond env
            let! trueNode, _ = compileExpr trueBranch env
            let! falseNode, _ = compileExpr falseBranch env
            let node =
                ConditionalExpression(
                    condNode, trueNode, falseNode)
            return node, env
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
