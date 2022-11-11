namespace CompilerDesign.Assignment6

type IdentifierDef<'tag> =
    {
        Name : string
        Tag : 'tag
    }

module IdentifierDef =

    let create name =
        {
            Name = name
            Tag = ()
        }

    let untag ident =
        create ident.Name
