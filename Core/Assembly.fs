namespace CompilerDesign.Core
 
open System.IO
open System.Reflection

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp

open Basic.Reference.Assemblies

module Assembly =

    /// Helper function corresponding to compile_prog in Lecture 3.
    /// https://course.ccs.neu.edu/cs4410sp22/lec_let-and-stack_notes.html
    let compile_prog assemblyName node =
        result {

            let compilation =
                CSharpCompilation
                    .Create(assemblyName)
                    .WithReferences(
                        Net60.SystemRuntime,
                        Net60.SystemConsole)

            let compilationUnit, mainTypeName =
                CompilationUnit.create compilation node
    #if DEBUG
            printfn "%A" <| compilationUnit.NormalizeWhitespace()
    #endif

            let emitResult =
                let compilation' =
                    let options =
                        CSharpCompilationOptions(OutputKind.ConsoleApplication)
                            .WithMainTypeName(mainTypeName)
                    compilation
                        .AddSyntaxTrees(compilationUnit.SyntaxTree)
                        .WithOptions(options)
                compilation'.Emit($"{assemblyName}.dll")
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
