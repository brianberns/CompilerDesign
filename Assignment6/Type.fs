﻿namespace CompilerDesign.Assignment6

type IdentifierDef<'tag> =
    {
        Name : string
        Tag : 'tag
    }

module IdentifierDef =

    let untag ident =
        {
            Name = ident.Name
            Tag = ()
        }

type Type<'tag> =

    /// No type specified. Will be inferred later.
    | TypeBlank of 'tag

    /// Type constant. E.g. "Int", "Bool".
    | TypeConstant of IdentifierDef<'tag>

    /// Type variable. E.g. "'a".
    | TypeVariable of IdentifierDef<'tag>

    /// Function type. E.g. "('a, Bool) -> Int".
    | TypeArrow of TypeArrowDef<'tag>

and TypeArrowDef<'tag> =
    {
        InputTypes : List<Type<'tag>>
        OutputType : Type<'tag>
        Tag : 'tag
    }

module Type =

    let rec untag = function
        | TypeBlank _ -> TypeBlank ()
        | TypeConstant def -> TypeConstant (IdentifierDef.untag def)
        | TypeVariable def -> TypeVariable (IdentifierDef.untag def)
        | TypeArrow def ->
            TypeArrow {
                InputTypes =
                    def.InputTypes |> List.map untag
                OutputType = untag def.OutputType
                Tag = ()
            }

    let rec unparse = function
        | TypeBlank _ -> "_"
        | TypeConstant def -> def.Name
        | TypeVariable def -> $"'{def.Name}"     // apostrophe is implicit
        | TypeArrow def ->
            let inputs =
                if def.InputTypes.IsEmpty then   // to-do: handle function with no inputs
                    failwith "Not supported"
                else
                    def.InputTypes
                        |> Seq.map unparse
                        |> String.concat ", "
            $"({inputs} -> {unparse def.OutputType})"

    let int = TypeConstant { Name = "Int"; Tag = () }
    let bool = TypeConstant { Name = "Bool"; Tag = () }

    let checkMissing typ =
        if typ = TypeBlank () then
            Error "Missing type"
        else Ok ()

    let mismatch expected actual =
        Error
            $"Expected: {unparse expected}, \
            Actual: {unparse actual}"
