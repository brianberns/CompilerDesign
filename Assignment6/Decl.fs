namespace CompilerDesign.Assignment6

type Decl<'tag> =
    {
        /// Name of function begin declared.
        Identifier : IdentifierDef<'tag>
        Parameters : List<IdentifierDef<'tag>>
        Scheme : Scheme<'tag>
        Body : Expr<'tag>
    }

module Decl =

    let unparse decl =
        let ident = decl.Identifier.Name
        let parms =
            decl.Parameters
                |> Seq.map (fun ident -> ident.Name)
                |> String.concat ", "
        let body = Expr.unparse decl.Body
        $"def {ident}({parms}):\n{body}"

type Program<'tag> =
    {
        Declarations : List<Decl<'tag>>
        Main : Expr<'tag>
    }

module Program =

    let unparse program =
        let decls =
            program.Declarations
                |> List.map Decl.unparse
                |> String.concat "\n"
        let main = Expr.unparse program.Main
        $"{decls}\n{main}"
