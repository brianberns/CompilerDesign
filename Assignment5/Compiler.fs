namespace CompilerDesign.Assignment5

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open type SyntaxFactory

open CompilerDesign.Core

module private Syntax =

    let numericLiteral (n : int) =
        LiteralExpression(
            SyntaxKind.NumericLiteralExpression,
            Literal(n))

    let boolLiteral flag =
        let kind =
            if flag then SyntaxKind.TrueLiteralExpression
            else SyntaxKind.FalseLiteralExpression
        LiteralExpression(kind)

    let by1 node kind =
        BinaryExpression(
            kind,
            node,
            numericLiteral 1)

    let isType node kind =
        BinaryExpression(
            SyntaxKind.IsExpression,
            node,
            PredefinedType(Token(kind)))

    let print node =
        InvocationExpression(IdentifierName("Print"))
            .WithArgumentList(
                ArgumentList(
                    SingletonSeparatedList(
                        Argument(node))))

    let not node =
        PrefixUnaryExpression(
            SyntaxKind.LogicalNotExpression,
            node)

module Compiler =

    let private compileNumber num (env : env) =
        let node =
            Syntax.numericLiteral num
                :> Syntax.ExpressionSyntax
        Ok (node, env)

    let private compileBool flag (env : env) =
        let node =
            Syntax.boolLiteral flag
                :> Syntax.ExpressionSyntax
        Ok (node, env)

    let private compileIdentifier ident env =
        Env.tryFind ident env
            |> Result.map (fun node -> node, env)

    module private rec Expr =

        let compile expr env : CompilerResult<_> =
            match expr with
                | LetExpr def ->
                    compileLet def.Bindings def.Expr env
                | Prim1Expr def ->
                    compilePrim1 def.Operator def.Expr env
                | Prim2Expr def ->
                    compilePrim2 def.Operator def.Left def.Right env
                | IfExpr def ->
                    compileIf def.Condition def.TrueBranch def.FalseBranch env
                | NumberExpr def ->
                    compileNumber def.Number env
                | IdentifierExpr def ->
                    compileIdentifier def.Name env
                | BoolExpr def ->
                    compileBool def.Flag env
                | ApplicationExpr def ->
                    // compileApplication def.Identifier def.Arguments
                    failwith "boom"

        let private compileLet bindings expr env =

            let rec loop (bindings : List<Binding<_>>) env=
                match bindings with
                    | binding :: tail ->
                        result {
                            let! node, env' =
                                compile binding.Expr env
                            let! env'' =
                                env' |> Env.tryAdd binding.Identifier.Name node
                            return! loop tail env''
                        }
                    | [] -> Ok env

            result {
                let! env' = loop bindings env
                return! compile expr env'
            }

        let private compilePrim1 op expr env =

            result {
                let! node, _ = compile expr env

                let prim1Node =
                    match op with
                        | Add1 ->
                            Syntax.by1 node SyntaxKind.AddExpression
                                :> Syntax.ExpressionSyntax
                        | Sub1 ->
                            Syntax.by1 node SyntaxKind.SubtractExpression
                        | Print ->
                            Syntax.print node
                        | IsBool ->
                            Syntax.isType node SyntaxKind.BoolKeyword
                        | IsNum ->
                            Syntax.isType node SyntaxKind.IntKeyword
                        | Not ->
                            Syntax.not node
                return prim1Node, env
            }

        let private compilePrim2 op left right env =
            let kind =
                match op with
                    | Plus -> SyntaxKind.AddExpression
                    | Minus -> SyntaxKind.SubtractExpression
                    | Times -> SyntaxKind.MultiplyExpression
                    | And -> SyntaxKind.LogicalAndExpression
                    | Or -> SyntaxKind.LogicalOrExpression
                    | Greater -> SyntaxKind.GreaterThanExpression
                    | GreaterEq -> SyntaxKind.GreaterThanOrEqualExpression
                    | Less -> SyntaxKind.LessThanExpression
                    | LessEq -> SyntaxKind.LessThanOrEqualExpression
                    | Eq -> SyntaxKind.EqualsExpression
            result {
                let! leftNode, _ = compile left env
                let! rightNode, _ = compile right env
                let node =
                    BinaryExpression(
                        kind,
                        leftNode,
                        rightNode)
                return node, env
            }

        let private compileIf cond trueBranch falseBranch env =
            result {

                let! condNode, _ = compile cond env
                let! trueNode, _ = compile trueBranch env
                let! falseNode, _ = compile falseBranch env

                let node =
                    ConditionalExpression(
                        condNode, trueNode, falseNode)

                return node, env
            }

    module private Decl =

        let compile decl =
            result {
                return MethodDeclaration(
                    returnType =
                        PredefinedType(
                            Token(SyntaxKind.IntKeyword)),
                    identifier = decl.Identifier.Name)
                        :> Syntax.MemberDeclarationSyntax
            }

    module private Program =

        let compile program =
            result {
                let! declNodes =
                    program.Declarations
                        |> List.map Decl.compile
                        |> Result.List.sequence
                        |> Result.map Seq.toArray
                let! mainNode, _ = Expr.compile program.Main Env.empty
                return mainNode, declNodes
            }

    let compile assemblyName text =
        result {
            let! program = Parser.parse text
            let! mainNode, declNodes = Program.compile program
            do! Compiler.compileWithMembers assemblyName mainNode declNodes
        }
