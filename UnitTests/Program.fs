open CompilerDesign.Assignment3

let text = "add1(x) + sub1(2) * add1(3)"
let actual = Parser.parse text
printfn "%A" actual
