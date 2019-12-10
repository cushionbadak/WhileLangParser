open BLang

let read_whole_file filename : string =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch; 
  s

let parse : string -> BLang.program list = 
  fun s -> 
    let lexbuf = Lexing.from_string s in
    let programs = Parser.programlist Lexer.start lexbuf in
    programs

let filenames : string list ref = ref []
let setFilenames s = filenames := !filenames @ [s]
let cmdParmas = []

let _ =
  let usage_msg = Printf.sprintf "Usage: %s <input-file>" Sys.argv.(0) in
  Arg.parse cmdParmas setFilenames usage_msg;
  let run (filename : string) = 
    let result = parse (read_whole_file filename) in
    List.iter (fun x -> print_endline (prog2str x)) result
    in
  List.iter run !filenames
