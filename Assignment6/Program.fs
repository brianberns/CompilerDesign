namespace CompilerDesign.Assignment6

type Program<'tag> =
    {
        DeclGroups : List<DeclGroup<'tag>>
        Main : Expr<'tag>
    }

module Program =

    let untag program =
        {
            DeclGroups =
                program.DeclGroups
                    |> List.map DeclGroup.untag
            Main = Expr.untag program.Main
        }

    let unparse program =
        let declGroups =
            program.DeclGroups
                |> List.map DeclGroup.unparse
                |> String.concat ""
        let main = Expr.unparse program.Main
        $"{declGroups}{main}"
