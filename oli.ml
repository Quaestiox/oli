open Lexer
open Util

let usage = "Usage: oli [option] {file}\n"

let print_usage () =
    print_endline "Usage: oli [OPTIONS] -o output input.c";
    print_endline "Options:";
    print_endline "  -o <file>     Output executable name";
    print_endline "  -t         Print lexer tokens only";
    print_endline "  -a       Print parser AST only";
    print_endline "  -h        Show this help message"   

let () = 
    print_endline "Compiling...";
    let source = "try.c" in
  
    (* read the source file *)
    let content = read_file source in

    (* lexer scan *)
    let tokens = scan content 0 in

    print_tokens tokens

