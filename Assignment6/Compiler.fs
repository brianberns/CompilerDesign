namespace CompilerDesign.Assignment6

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

    module private rec Expression =

        let compile env = function
            | LetExpr def -> (compileLet env def : CompilerResult<_>)
            | Prim1Expr def -> compilePrim1 env def
            | Prim2Expr def -> compilePrim2 env def
            | IfExpr def -> compileIf env def
            | NumberExpr def -> compileNumber env def
            | IdentifierExpr def -> compileIdentifier env def
            | BoolExpr def -> compileBool env def
            | ApplicationExpr def -> compileApplication env def
            | AnnotationExpr def -> compile env def.Expr

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

        let private compileLet env (def : LetDef<_>) =
            result {
                let! env' =
                    (env, def.Bindings)
                        ||> Result.List.foldM (fun acc binding ->
                            result {
                                let! node, acc' =
                                    compile acc binding.Expr
                                return! acc'
                                    |> Env.tryAdd
                                        binding.Identifier.Name
                                        node
                            })
                return! compile env' def.Expr
            }

        let private compilePrim1 env (def : Prim1Def<_>) =
            result {
                let! node, _ = compile env def.Expr
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

        let private compilePrim2 env (def: Prim2Def<_>) =
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
                let! leftNode, _ = compile env def.Left
                let! rightNode, _ = compile env def.Right
                let node =
                    BinaryExpression(
                        kind,
                        leftNode,
                        rightNode)
                return node, env
            }

        let private compileIf env (def : IfDef<_>) =
            result {

                let! condNode, _ = compile env def.Condition
                let! trueNode, _ = compile env def.TrueBranch
                let! falseNode, _ = compile env def.FalseBranch

                let node =
                    ConditionalExpression(
                        condNode, trueNode, falseNode)

                return node, env
            }

        let private compileApplication env (def : ApplicationDef<_>) =
            result {

                let! argsNode =
                    def.Arguments
                        |> Result.List.traverse (fun expr ->
                            compile env expr
                                |> Result.map (fst >> Argument))
                        |> Result.map SeparatedList

                let node =
                    InvocationExpression(
                        IdentifierName(def.Identifier.Name))
                        .WithArgumentList(ArgumentList(argsNode))

                return node, env
            }

    type Syntax.MethodDeclarationSyntax with
        member node.MaybeWithTypeParameterList(
            typeParameterList : Syntax.TypeParameterListSyntax) =
            if typeParameterList.Parameters.Count > 0 then
                node.WithTypeParameterList(typeParameterList)
            else node

    module private Decl =

        let private predefinedTypeMap =
            Map [
                Type.int, SyntaxKind.IntKeyword
                Type.bool, SyntaxKind.BoolKeyword
            ]

        let private compileType typ =
            result {
                do! TypeCheck.Type.checkMissing typ
                match typ with
                    | TypeConstant _ as typ ->
                        let kind = predefinedTypeMap[typ]
                        return (PredefinedType(Token(kind)) : Syntax.TypeSyntax)
                    | TypeVariable def ->
                        return IdentifierName(def.Name)
                    | _ -> return! Error "Unexpected type"
            }

        let private compileParameter parm typ =
            result {
                let! typeNode = compileType typ
                return Parameter(
                    Identifier(parm.Name))
                        .WithType(typeNode)
            }

        let compile decl =
            result {

                let! typedParms, outputType = Decl.getSignature decl
                let! returnType = compileType outputType
                let typeParmNodes =
                    decl.Scheme.TypeVariableIdents
                        |> Seq.map (fun tvIdent ->
                            TypeParameter(
                                Identifier(tvIdent.Name)))
                let! env =
                    (Env.empty, typedParms)
                        ||> Result.List.foldM (fun acc (parm, _) ->
                            result {
                                let node = IdentifierName(parm.Name)
                                return! acc
                                    |> Env.tryAdd parm.Name node
                            })
                let! parmNodes =
                    typedParms
                        |> Result.List.traverse (fun (parm, typ) ->
                            compileParameter parm typ)
                let! bodyNode, _ = Expression.compile env decl.Body

                return MethodDeclaration(
                    returnType = returnType,
                    identifier = decl.Identifier.Name)
                    .AddModifiers(
                        Token(SyntaxKind.StaticKeyword))
                    .MaybeWithTypeParameterList(
                        TypeParameterList(SeparatedList(typeParmNodes)))
                    .WithParameterList(
                        ParameterList(SeparatedList(parmNodes)))
                    .WithBody(
                        Block(ReturnStatement(bodyNode)))
            }

    module private DeclGroup =

        let compile group =
            group.Decls
                |> Result.List.traverse Decl.compile

    module private Program =

        let compile program =
            result {
                let! declNodes =
                    program.DeclGroups
                        |> Result.List.traverse DeclGroup.compile
                        |> Result.map (Seq.concat >> Seq.toArray)
                let! mainNode, _ =
                    Expression.compile Env.empty program.Main
                return mainNode, declNodes
            }

    let compile assemblyName text =
        result {
            let! program = Parser.parse text
            let! program' = TypeInfer.annotate program
            do! TypeCheck.validate program'
            let! mainNode, methodNodes =
                Program.compile program'
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
