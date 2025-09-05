open Lexer
open Ast
open Parser
open Codegen
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
    let output = "try" in
  
    (* read the source file *)
    let content = read_file source in

    (* lexer scan *)
    let tokens = scan content 0 in

    print_tokens tokens;

    (* grammatical analysis *)
    let parser_state = create_parser tokens in  
    let ast = parse parser_state in
    print_ast ast;

    (* generate assembly code *)
    let asm_code = gen_program ast in
    write_file (output ^ ".s") asm_code;
    Printf.printf "Generated assembly code\n";

    let _ = Sys.command ("as -o " ^ output ^ ".o " ^ output ^ ".s") in
    let _ = Sys.command ("ld -o " ^ output ^ " " ^ output ^ ".o") in
    print_endline "done.\n"

    

