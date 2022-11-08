open CompilerDesign.Assignment6

for (KeyValue(ident, scheme)) in SchemeEnvironment.initial do
    printfn $"{ident.Name}: {Scheme.unparse scheme}"
