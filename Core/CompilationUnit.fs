﻿namespace CompilerDesign.Core

open Microsoft.CodeAnalysis.CSharp
open type SyntaxFactory

module private CompilationUnit =

    (*
    static object Print(object obj)
    {
        System.Console.WriteLine(obj);
        return obj;
    }
    *)
    let private printMethod =
        MethodDeclaration(
            PredefinedType(
                Token(SyntaxKind.ObjectKeyword)),
            Identifier("Print"))
            .WithModifiers(
                TokenList(
                    Token(SyntaxKind.StaticKeyword)))
            .WithParameterList(
                ParameterList(
                    SingletonSeparatedList(
                        Parameter(
                            Identifier("obj"))
                            .WithType(
                                PredefinedType(
                                    Token(SyntaxKind.ObjectKeyword))))))
            .WithBody(
                Block(
                    ExpressionStatement(
                        InvocationExpression(
                            MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                MemberAccessExpression(
                                    SyntaxKind.SimpleMemberAccessExpression,
                                    IdentifierName("System"),
                                    IdentifierName("Console")),
                                IdentifierName("WriteLine")))
                            .WithArgumentList(
                                ArgumentList(
                                    SingletonSeparatedList(
                                        Argument(
                                            IdentifierName("obj")))))),
                    ReturnStatement(
                        IdentifierName("obj"))))

    (*
        static int our_code_starts_here()
        {
            return node;
        }
    *)
    let private our_code_starts_here node =
        MethodDeclaration(
            returnType =
                PredefinedType(
                    Token(SyntaxKind.IntKeyword)),
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

    let create assemblyName node =
        let classNode =
            ClassDeclaration($"{assemblyName}Type")
                .AddModifiers(
                    Token(SyntaxKind.StaticKeyword))
                .AddMembers(
                    printMethod,
                    our_code_starts_here node,
                    mainMethod)
        let namespaceNode =
            NamespaceDeclaration(
                IdentifierName(assemblyName : string))
                .AddMembers(classNode)
        let compilationUnit =
            CompilationUnit().AddMembers(namespaceNode)
        let mainTypeName =
            $"{namespaceNode.Name}.{classNode.Identifier}"
        compilationUnit, mainTypeName
