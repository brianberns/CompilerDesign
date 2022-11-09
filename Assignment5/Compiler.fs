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

type private ArityEnvironment = Map<string, int>

module ArityEnvironment =

    let empty : ArityEnvironment =
        Map.empty

    let tryAdd name arity (aenv : ArityEnvironment) =
        if Map.containsKey name aenv then
            Error $"Function already exists: {name}"
        else
            let env : ArityEnvironment = Map.add name arity aenv
            Ok env

    let tryFind name (aenv : ArityEnvironment) =
        match Map.tryFind name aenv with
            | Some arity -> Ok arity
            | None -> Error $"Function not found: {name}"

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

        let compile env aenv expr : CompilerResult<_> =
            match expr with
                | LetExpr def ->
                    compileLet env aenv def.Bindings def.Expr
                | Prim1Expr def ->
                    compilePrim1 env aenv def.Operator def.Expr
                | Prim2Expr def ->
                    compilePrim2 env aenv def.Operator def.Left def.Right
                | IfExpr def ->
                    compileIf env aenv def.Condition def.TrueBranch def.FalseBranch
                | NumberExpr def ->
                    compileNumber env def.Number
                | IdentifierExpr def ->
                    compileIdentifier env def.Name
                | BoolExpr def ->
                    compileBool env def.Flag
                | ApplicationExpr def ->
                    compileApplication env aenv def.Identifier def.Arguments

        let private compileLet env aenv bindings expr =
            result {
                let! env' =
                    (env, bindings)
                        ||> Result.List.foldM (fun acc binding ->
                            result {
                                let! node, acc' =
                                    compile acc aenv binding.Expr
                                return! acc'
                                    |> Env.tryAdd
                                        binding.Identifier.Name
                                        node
                            })
                return! compile env' aenv expr
            }

        let private compilePrim1 env aenv op expr =
            result {
                let! node, _ = compile env aenv expr

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

        let private compilePrim2 env aenv op left right =
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
                let! leftNode, _ = compile env aenv left
                let! rightNode, _ = compile env aenv right
                let node =
                    BinaryExpression(
                        kind,
                        leftNode,
                        rightNode)
                return node, env
            }

        let private compileIf env aenv cond trueBranch falseBranch =
            result {

                let! condNode, _ = compile env aenv cond
                let! trueNode, _ = compile env aenv trueBranch
                let! falseNode, _ = compile env aenv falseBranch

                let node =
                    ConditionalExpression(
                        condNode, trueNode, falseNode)

                return node, env
            }

        let private compileApplication env aenv ident args =
            result {

                let! arity = ArityEnvironment.tryFind ident.Name aenv
                if arity <> args.Length then
                    return! Error $"Arity mismatch: expected {arity}, actual {args.Length}"

                let! argsNode =
                    args
                        |> List.map (fun expr ->
                            compile env aenv expr
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

        let compile aenv decl =
            result {

                let! env =
                    (Env.empty, decl.Parameters)
                        ||> Result.List.foldM (fun acc parm ->
                            result {
                                let node = IdentifierName(parm.Name)
                                return! acc
                                    |> Env.tryAdd parm.Name node
                            })

                let parmNodes =
                    decl.Parameters
                        |> List.map (fun parm ->
                            Parameter(Identifier(parm.Name))
                                .WithType(
                                    PredefinedType(
                                        Token(SyntaxKind.IntKeyword))))   // ugh

                let! aenv' =
                    ArityEnvironment.tryAdd
                        decl.Identifier.Name
                        decl.Parameters.Length
                        aenv
                let! bodyNode, _ = Expr.compile env aenv' decl.Body

                let declNode =
                    MethodDeclaration(
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

                return declNode, aenv'
            }

    module private Program =

        let compile program =
            result {
                let! declNodes, aenv =
                    ((List.empty, ArityEnvironment.empty), program.Declarations)
                        ||> Result.List.foldM (fun (declNodes, aenv) decl ->
                            result {
                                let! declNode, aenv' = Decl.compile aenv decl
                                return declNode :: declNodes, aenv'
                            })
                let! mainNode, _ =
                    Expr.compile Env.empty aenv program.Main
                return mainNode, List.rev declNodes
            }

    let compile assemblyName text =
        result {
            let! program = Parser.parse text
            let! mainNode, methodNodes = Program.compile program
            let memberNodes =
                methodNodes
                    |> Seq.cast<Syntax.MemberDeclarationSyntax>
                    |> Seq.toArray
            do!
                Compiler.compileWithMembers
                    assemblyName
                    mainNode
                    memberNodes
        }
