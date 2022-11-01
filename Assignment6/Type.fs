namespace CompilerDesign.Assignment6

type Type<'tag> =

    /// No type specified. Will be inferred later.
    | TypeBlank of 'tag

    /// Type constant. E.g. "Int", "Bool".
    | TypeConstant of string * 'tag

    /// Type variable. E.g. "'a".
    | TypeVariable of string * 'tag

    /// Function type. E.g. "('a, Bool) -> Int".
    | TypeArrow of TypeArrowDef<'tag>

    /// Type application. E.g. "List<Int>".
    | TypeApplication of TypeApplicationDef<'tag>

and TypeArrowDef<'tag> =
    {
        InputTypes : List<Type<'tag>>
        OutputType : Type<'tag>
        Tag : 'tag
    }

and TypeApplicationDef<'tag> =
    {
        Type : Type<'tag>
        TypeArguments : List<Type<'tag>>
        Tag : 'tag
    }
