namespace CompilerDesign.Core

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open type SyntaxFactory

open CompilerDesign.Core

module CompilationUnit =

    (*
        static long our_code_starts_here()
        {
            return node;
        }
    *)
    let private our_code_starts_here node =
        MethodDeclaration(
            returnType =
                PredefinedType(
                    Token(SyntaxKind.LongKeyword)),
            identifier = "our_code_starts_here")
            .AddModifiers(
                Token(SyntaxKind.StaticKeyword))
            .WithBody(
                Block(
                    ReturnStatement(node)))

    (*
        static int Main()
        {
            var result = our_code_starts_here();
            System.Console.Write(result);
            return 0;
        }
    *)
    let private mainMethod =
        MethodDeclaration(
            returnType =
                PredefinedType(
                    Token(SyntaxKind.IntKeyword)),
            identifier = "Main")
            .AddModifiers(
                Token(SyntaxKind.StaticKeyword))
            .WithBody(
                Block(
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
                                        Identifier("result"))
                                        .WithInitializer(
                                            EqualsValueClause(
                                                InvocationExpression(
                                                    IdentifierName("our_code_starts_here"))))))),
                    ExpressionStatement(
                        InvocationExpression(
                            MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                MemberAccessExpression(
                                    SyntaxKind.SimpleMemberAccessExpression,
                                    IdentifierName("System"),
                                    IdentifierName("Console")),
                                IdentifierName("Write")))
                            .WithArgumentList(
                                ArgumentList(
                                    SingletonSeparatedList(
                                        Argument(
                                            IdentifierName("result")))))),
                    ReturnStatement(
                        LiteralExpression(
                            SyntaxKind.NumericLiteralExpression,
                            Literal(0)))))

    let create (compilation : Compilation) node =
        let assemblyName = compilation.AssemblyName
        let classNode =
            ClassDeclaration($"{assemblyName}Type")
                .AddModifiers(
                    Token(SyntaxKind.StaticKeyword))
                .AddMembers(
                    our_code_starts_here node,
                    mainMethod)
        let namespaceNode =
            NamespaceDeclaration(
                IdentifierName(assemblyName))
                .AddMembers(classNode)
        let compilationUnit =
            CompilationUnit().AddMembers(namespaceNode)
        let mainTypeName =
            $"{namespaceNode.Name}.{classNode.Identifier}"
        compilationUnit, mainTypeName
