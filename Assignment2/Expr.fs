namespace Assignment2

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open type SyntaxFactory

type expr = int64

module private Expr =

    let compile_expr (e : expr) =
        LiteralExpression(
            SyntaxKind.NumericLiteralExpression,
            Literal(e))

module private CompilationUnit =

    (*
        static long our_code_starts_here()
        {
            return e;
        }
    *)
    let private our_code_starts_here e =
        MethodDeclaration(
            returnType =
                PredefinedType(
                    Token(SyntaxKind.LongKeyword)),
            identifier = "our_code_starts_here")
            .AddModifiers(
                Token(SyntaxKind.StaticKeyword))
            .WithBody(
                Block(
                    Expr.compile_expr e
                        |> ReturnStatement))

    (*
        static int Main()
        {
            var result = our_code_starts_here();
            System.Console.WriteLine(result);
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
                                IdentifierName("WriteLine")))
                            .WithArgumentList(
                                ArgumentList(
                                    SingletonSeparatedList(
                                        Argument(
                                            IdentifierName("result")))))),
                    ReturnStatement(
                        LiteralExpression(
                            SyntaxKind.NumericLiteralExpression,
                            Literal(0)))))

    let generate (compilation : Compilation) e =

        let assemblyName = compilation.AssemblyName
        let classNode =
            ClassDeclaration($"{assemblyName}Type")
                .AddModifiers(
                    Token(SyntaxKind.StaticKeyword))
                .AddMembers(
                    our_code_starts_here e,
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

 module Assembly =
 
    open System.IO
    open System.Reflection

    open Basic.Reference.Assemblies

    let compile_prog expr =

        let assemblyName = "Adder"

        let compilation =
            CSharpCompilation
                .Create(assemblyName)
                .WithReferences(
                    Net60.SystemRuntime,
                    Net60.SystemConsole)

        let compilationUnit, mainTypeName =
            CompilationUnit.generate compilation expr
#if DEBUG
        printfn "%A" <| compilationUnit.NormalizeWhitespace()
#endif

        let result =
            let compilation' =
                let options =
                    CSharpCompilationOptions(OutputKind.ConsoleApplication)
                        .WithMainTypeName(mainTypeName)
                compilation
                    .AddSyntaxTrees(compilationUnit.SyntaxTree)
                    .WithOptions(options)
            compilation'.Emit($"{assemblyName}.dll")
        if result.Success then
            let sourcePath =
                Path.Combine(
                    Path.GetDirectoryName(
                        Assembly.GetExecutingAssembly().Location),
                    "App.runtimeconfig.json")
            File.Copy(
                sourcePath,
                $"{assemblyName}.runtimeconfig.json",
                overwrite = true)
            Array.empty
        else
            result.Diagnostics
                |> Seq.map string
                |> Seq.toArray
