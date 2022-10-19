namespace Assignment2

type expr = int64

module Expr =

    open System.Reflection.Emit

    let compile_expr (ilGen : ILGenerator) (e : expr) =
        ilGen.Emit(OpCodes.Ldc_I8, e)

module Program =

    open System.Reflection
    open System.Reflection.Emit
    open Lokad.ILPack

    let compile_prog (e : expr) =

        let asmBld =
            let asmName = AssemblyName(Name = "Adder")
            AssemblyBuilder.DefineDynamicAssembly(
                asmName,
                AssemblyBuilderAccess.Run)

        let typBld =
            let modBld =
                asmBld.DefineDynamicModule("AdderModule")
            modBld.DefineType(
                "AdderType",
                TypeAttributes.Abstract   // attributes for a C# static class
                    ||| TypeAttributes.Sealed
                    ||| TypeAttributes.BeforeFieldInit)

        let ilGen =
            let mthBld =
                typBld.DefineMethod("AdderMethod", MethodAttributes.Static)
            mthBld.GetILGenerator()

        Expr.compile_expr ilGen e
        ilGen.Emit(OpCodes.Ret)

        typBld.CreateType() |> ignore

        let asmGen = AssemblyGenerator()
        asmGen.GenerateAssembly(asmBld, "Adder.dll")
