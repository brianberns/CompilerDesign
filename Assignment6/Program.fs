namespace CompilerDesign.Assignment6

type Program<'tag> =
    {
        DeclGroups : List<DeclGroup<'tag>>
        Main : Expression<'tag>
    }

module Program =

    let untag program =
        {
            DeclGroups =
                program.DeclGroups
                    |> List.map DeclGroup.untag
            Main = Expression.untag program.Main
        }

    let unparse program =
        let declGroups =
            program.DeclGroups
                |> List.map DeclGroup.unparse
                |> String.concat ""
        let main = Expression.unparse program.Main
        $"{declGroups}{main}"
