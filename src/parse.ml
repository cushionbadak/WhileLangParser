open WhileLang

let parse : string -> WhileLang.program list = fun s ->
  let lexbuf = Lexing.from_string s in
  let programs : WhileLang.program list = Parser.programlist Lexer.start lexbuf in
  programs