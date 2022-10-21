﻿namespace Assignment2

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open type SyntaxFactory

open Assignment1.ResultBuilder

module CompilationUnit =

    (*
        static long our_code_starts_here()
        {
            return e;
        }
    *)
    let private our_code_starts_here e =
        result {
            let! node = Expr.compile e
            return MethodDeclaration(
                returnType =
                    PredefinedType(
                        Token(SyntaxKind.LongKeyword)),
                identifier = "our_code_starts_here")
                .AddModifiers(
                    Token(SyntaxKind.StaticKeyword))
                .WithBody(
                    Block(
                        ReturnStatement(node)))
        }

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

    let create (compilation : Compilation) e =
        result {

            let assemblyName = compilation.AssemblyName
            let! ourMethod = our_code_starts_here e
            let classNode =
                ClassDeclaration($"{assemblyName}Type")
                    .AddModifiers(
                        Token(SyntaxKind.StaticKeyword))
                    .AddMembers(
                        ourMethod,
                        mainMethod)

            let namespaceNode =
                NamespaceDeclaration(
                    IdentifierName(assemblyName : string))
                    .AddMembers(classNode)

            let compilationUnit =
                CompilationUnit().AddMembers(namespaceNode)
            let mainTypeName =
                $"{namespaceNode.Name}.{classNode.Identifier}"
            return compilationUnit, mainTypeName
        }
