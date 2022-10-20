let diagnostics = Assignment2.Assembly.compile_prog 87L
for diagnostic in diagnostics do
    printfn "%s" diagnostic
