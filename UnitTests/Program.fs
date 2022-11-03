open CompilerDesign.Assignment6

(*
let text = "def j(): z k"
let parsed = Parser.parse text
*)
let parsed : Result<_, unit> =
    {
        Declarations =
            [
                {
                    Identifier = { Name = "z"; Tag = () }
                    Parameters = []
                    Scheme =
                        {
                            Identifiers = [{ Name = "n"; Tag = () }]
                            Type =
                                TypeArrow {
                                    InputTypes = []
                                    OutputType =
                                        TypeArrow {
                                            InputTypes = []
                                            OutputType =
                                                TypeConstant { Name = "o"; Tag = () }
                                            Tag = ()
                                        }
                                    Tag = ()
                                }
                            Tag = ()
                        }
                    Body =
                        Prim1Expr {
                            Operator = Not
                            TypeArguments = []
                            Expr = NumberExpr { Number = 0; Tag = () }
                            Tag = ()
                        }
                }
            ]
        Main = IdentifierExpr { Name = "s"; Tag = () }
    } |> Ok

printfn "%A" parsed
match parsed with
    | Ok program ->
        printfn "%A" (Program.unparse program)
    | _ -> ()
