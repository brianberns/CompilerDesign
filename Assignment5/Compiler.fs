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

    let private compileNumber (env : env) num =
        let node =
            Syntax.numericLiteral num
                :> Syntax.ExpressionSyntax
        Ok (node, env)

    let private compileBool (env : env) flag =
        let node =
            Syntax.boolLiteral flag
                :> Syntax.ExpressionSyntax
        Ok (node, env)

    let private compileIdentifier env ident =
        Env.tryFind ident env
            |> Result.map (fun node -> node, env)

    module private rec Expr =

        let compile env expr : CompilerResult<_> =
            match expr with
                | LetExpr def ->
                    compileLet env def.Bindings def.Expr
                | Prim1Expr def ->
                    compilePrim1 env def.Operator def.Expr
                | Prim2Expr def ->
                    compilePrim2 env def.Operator def.Left def.Right
                | IfExpr def ->
                    compileIf env def.Condition def.TrueBranch def.FalseBranch
                | NumberExpr def ->
                    compileNumber env def.Number
                | IdentifierExpr def ->
                    compileIdentifier env def.Name
                | BoolExpr def ->
                    compileBool env def.Flag
                | ApplicationExpr def ->
                    compileApplication env def.Identifier def.Arguments

        let private compileLet env bindings expr =

            let folder env (binding : Binding<_>) =
                result {
                    let! node, env' =
                        compile env binding.Expr
                    return! env'
                        |> Env.tryAdd
                            binding.Identifier.Name
                            node
                }

            result {
                let! env' = Result.List.foldM folder env bindings
                return! compile env' expr
            }

        let private compilePrim1 env op expr =
            result {
                let! node, _ = compile env expr

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

        let private compilePrim2 env op left right =
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
                let! leftNode, _ = compile env left
                let! rightNode, _ = compile env right
                let node =
                    BinaryExpression(
                        kind,
                        leftNode,
                        rightNode)
                return node, env
            }

        let private compileIf env cond trueBranch falseBranch =
            result {

                let! condNode, _ = compile env cond
                let! trueNode, _ = compile env trueBranch
                let! falseNode, _ = compile env falseBranch

                let node =
                    ConditionalExpression(
                        condNode, trueNode, falseNode)

                return node, env
            }

        let private compileApplication env ident args =
            result {

                let! argsNode =
                    args
                        |> List.map (fun expr ->
                            compile env expr
                                |> Result.map (fst >> Argument))
                        |> Result.List.sequence
                        |> Result.map SeparatedList

                let node =
                    InvocationExpression(
                        IdentifierName(ident.Name))
                        .WithArgumentList(ArgumentList(argsNode))

                return node, env
            }

    module private Decl =

        let private compileParameter _env parm =
            result {
                return Parameter(
                    Identifier(parm.Name))
                    .WithType(
                        PredefinedType(
                            Token(SyntaxKind.IntKeyword)))   // ugh
            }

        let compile env decl =

            let folder env parm =
                result {
                    let node = IdentifierName(parm.Name)
                    return! env
                        |> Env.tryAdd parm.Name node
                }

            result {

                let! env' =
                    Result.List.foldM folder env decl.Parameters
                let! parmNodes =
                    decl.Parameters
                        |> List.map (compileParameter env)
                        |> Result.List.sequence
                let! bodyNode, _ = Expr.compile env' decl.Body

                return MethodDeclaration(
                    returnType =
                        PredefinedType(
                            Token(SyntaxKind.IntKeyword)),
                    identifier = decl.Identifier.Name)
                    .AddModifiers(
                        Token(SyntaxKind.StaticKeyword))
                    .WithParameterList(
                        ParameterList(SeparatedList(parmNodes)))
                    .WithBody(
                        Block(ReturnStatement(bodyNode)))
            }

    module private Program =

        let compile env program =
            result {
                let! declNodes =
                    program.Declarations
                        |> List.map (Decl.compile env)
                        |> Result.List.sequence
                        |> Result.map Seq.toArray
                let! mainNode, _ = Expr.compile Env.empty program.Main
                return mainNode, declNodes
            }

    let compile assemblyName text =
        result {
            let! program = Parser.parse text
            let! mainNode, methodNodes =
                Program.compile Env.empty program
            let memberNodes =
                methodNodes
                    |> Array.map (fun node ->
                        node :> Syntax.MemberDeclarationSyntax)
            do!
                Compiler.compileWithMembers
                    assemblyName
                    mainNode
                    memberNodes
        }
