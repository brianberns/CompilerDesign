open CompilerDesign.Assignment4

let text = "true + 0"
let actual = Parser.parse text
printfn "%A" actual
