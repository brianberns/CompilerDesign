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

    let private compileNumber (env : env) (def : NumberDef<_>) =
        let node =
            Syntax.numericLiteral def.Number
                :> Syntax.ExpressionSyntax
        Ok (node, env)

    let private compileBool (env : env) (def : BoolDef<_>) =
        let node =
            Syntax.boolLiteral def.Flag
                :> Syntax.ExpressionSyntax
        Ok (node, env)

    let private compileIdentifier env (def : IdentifierDef<_>) =
        Env.tryFind def.Name env
            |> Result.map (fun node -> node, env)

    module private rec Expr =

        let compile env aenv expr : CompilerResult<_> =
            match expr with
                | LetExpr def ->
                    compileLet env aenv def
                | Prim1Expr def ->
                    compilePrim1 env aenv def
                | Prim2Expr def ->
                    compilePrim2 env aenv def
                | IfExpr def ->
                    compileIf env aenv def
                | NumberExpr def ->
                    compileNumber env def
                | IdentifierExpr def ->
                    compileIdentifier env def
                | BoolExpr def ->
                    compileBool env def
                | ApplicationExpr def ->
                    compileApplication env aenv def

        let private compileLet env aenv (def : LetDef<_>) =
            result {
                let! env' =
                    (env, def.Bindings)
                        ||> Result.List.foldM (fun acc binding ->
                            result {
                                let! node, acc' =
                                    compile acc aenv binding.Expr
                                return! acc'
                                    |> Env.tryAdd
                                        binding.Identifier.Name
                                        node
                            })
                return! compile env' aenv def.Expr
            }

        let private compilePrim1 env aenv (def : Prim1Def<_>) =
            result {
                let! node, _ = compile env aenv def.Expr

                let prim1Node =
                    match def.Operator with
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

        let private compilePrim2 env aenv (def : Prim2Def<_>) =
            let kind =
                match def.Operator with
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
                let! leftNode, _ = compile env aenv def.Left
                let! rightNode, _ = compile env aenv def.Right
                let node =
                    BinaryExpression(
                        kind,
                        leftNode,
                        rightNode)
                return node, env
            }

        let private compileIf env aenv (def : IfDef<_>) =
            result {

                let! condNode, _ = compile env aenv def.Condition
                let! trueNode, _ = compile env aenv def.TrueBranch
                let! falseNode, _ = compile env aenv def.FalseBranch

                let node =
                    ConditionalExpression(
                        condNode, trueNode, falseNode)

                return node, env
            }

        let private compileApplication env aenv (def : ApplicationDef<_>) =
            result {

                let! arity =
                    ArityEnvironment.tryFind def.Identifier.Name aenv
                if arity <> def.Arguments.Length then
                    return! Error $"Arity mismatch: \
                        expected {arity}, \
                        actual {def.Arguments.Length}"

                let! argsNode =
                    def.Arguments
                        |> List.map (fun expr ->
                            compile env aenv expr
                                |> Result.map (fst >> Argument))
                        |> Result.List.sequence
                        |> Result.map SeparatedList

                let node =
                    InvocationExpression(
                        IdentifierName(def.Identifier.Name))
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
