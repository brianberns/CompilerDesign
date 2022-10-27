namespace CompilerDesign.Core

open System.IO
open System.Reflection

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp

open Basic.Reference.Assemblies

module Compiler =

    /// Helper function corresponding to compile_prog in Lecture 3.
    /// https://course.ccs.neu.edu/cs4410sp22/lec_let-and-stack_notes.html
    let compile_prog assemblyName declNodes mainNode =

        let emitResult =

            let compilationUnit, mainTypeName =
                CompilationUnit.create assemblyName declNodes mainNode
#if DEBUG
            printfn "%A" <| compilationUnit.NormalizeWhitespace()
#endif
            let compilation =
                let options =
                    CSharpCompilationOptions(OutputKind.ConsoleApplication)
                        .WithMainTypeName(mainTypeName)
                CSharpCompilation
                    .Create(assemblyName)
                    .WithReferences(
                        Net60.SystemRuntime,
                        Net60.SystemConsole)
                    .AddSyntaxTrees(compilationUnit.SyntaxTree)
                    .WithOptions(options)
            compilation.Emit($"{assemblyName}.dll")

        result {
            if emitResult.Success then
                let sourcePath =
                    Path.Combine(
                        Path.GetDirectoryName(
                            Assembly.GetExecutingAssembly().Location),
                        "App.runtimeconfig.json")
                File.Copy(
                    sourcePath,
                    $"{assemblyName}.runtimeconfig.json",
                    overwrite = true)
            else
                return! emitResult.Diagnostics
                    |> Seq.map string
                    |> Seq.toArray
                    |> Error
        }
