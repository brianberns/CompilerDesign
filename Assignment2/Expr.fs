namespace Assignment2

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open type SyntaxFactory

open Assignment1

type prim1 =
    | Add1
    | Sub1

type expr<'a> =
    | Number of int64 * 'a
    | Prim1 of prim1 * expr<'a> * 'a
    | Let of (*bindings*) List<string * expr<'a>> * expr<'a> * 'a
    | Id of string * 'a

type env = Map<string, Syntax.ExpressionSyntax>

module Env =

    let add name node (env : env) : env =
        Map.add name node env

type CompilerResult<'a> = Result<'a, string[]>

[<AutoOpen>]
module private CompilerResult =

    let error<'a> msg : CompilerResult<'a> =
        Error [| msg |]

module Expr =

    let private numericLiteral (n : int64) =
        LiteralExpression(
            SyntaxKind.NumericLiteralExpression,
            Literal(n))

    let private declareVariable id node =
        LocalDeclarationStatement(
            VariableDeclaration(
                IdentifierName(
                    Identifier(
                        TriviaList(),
                        SyntaxKind.VarKeyword,
                        "var",
                        "var",
                        TriviaList())))
                .WithVariables(
                    SingletonSeparatedList(
                        VariableDeclarator(
                            Identifier(id))
                            .WithInitializer(
                                EqualsValueClause(
                                    node)))))

    let private compileNumber n (env : env) =
        let node =
            numericLiteral n
                :> Syntax.ExpressionSyntax
        Ok (node, env)

    let private compileId id (env : env) =
        match Map.tryFind id env with
            | Some node -> Ok (node, env)
            | None -> error $"Unbound identifier: {id}"

    let rec private compileExp exp (env : env) : Result<Syntax.ExpressionSyntax * env, _> =
        match exp with
            | Number (n, _) -> compileNumber n env
            | Prim1 (op, e, _) -> compilePrim1 op e env
            | Let (bindings, e, _) -> compileLet bindings e env
            | Id (id, _) -> compileId id env

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
                        let env'' = env' |> Env.add name node
                        return! loop tail env''
                    }
                | [] -> Ok env

        result {
            let! env' = loop bindings env
            return! compileExp exp env'
        }

    let compile exp =
        result {
            let! node, env = compileExp exp Map.empty
            return node
        }
