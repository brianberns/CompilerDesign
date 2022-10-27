namespace CompilerDesign.Assignment5

type Decl<'tag> =
    {
        /// Name of function begin declared.
        Identifier : IdentifierDef<'tag>
        Parameters : List<IdentifierDef<'tag>>
        Body : Expr<'tag>
    }

type Program<'tag> =
    {
        Declarations : List<Decl<'tag>>
        Main : Expr<'tag>
    }
