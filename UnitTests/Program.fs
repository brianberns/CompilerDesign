open CompilerDesign.Assignment6

open CompilerDesign.Core

let program =
    // (int, int) -> int
    """
    def add(x, y) : x + y
    0
    """
        |> Parser.parse
        |> Result.get

result {
    let decl =
        program.DeclGroups[0].Decls[0]
            |> Decl.untag

    let! _, typ =
        TypeInfer.Decl.infer
            SchemeEnvironment.initial
            Map.empty
            decl

    return Type.unparse typ
} |> printfn "%A"
