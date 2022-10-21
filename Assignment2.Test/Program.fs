open Assignment2

match Compiler.run "sub1(add1(add1(42)))" with
    | Ok n -> printfn $"Success: {n}"
    | Error msgs ->
        for msg in msgs do
            printfn $"{msg}"
