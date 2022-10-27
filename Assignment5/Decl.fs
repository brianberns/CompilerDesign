namespace CompilerDesign.Assignment5

type Decl<'tag> =
    {
        /// Name of function begin declared.
        Identifier : string
        Parameters : List<string>
        Body : Expr<'tag>
        Tag : 'tag
    }

type Program<'tag> =
    {
        Declarations : List<Decl<'tag>>
        Main : Expr<'tag>
    }
