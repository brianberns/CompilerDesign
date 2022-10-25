open CompilerDesign.Assignment4

let text = "true"
let actual = Parser.parse text
printfn "%A" actual
