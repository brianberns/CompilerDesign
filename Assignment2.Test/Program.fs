open Assignment2

match Compiler.run "(sub1 (add1 (sub1 5)))" with
    | Ok str -> printfn $"{str}"
    | Error msgs ->
        for msg in msgs do
            printfn $"{msg}"
