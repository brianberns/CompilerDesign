namespace CompilerDesign.Assignment6

/// An identifier, such as the name of a value or function.
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
