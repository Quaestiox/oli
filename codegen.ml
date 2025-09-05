(* generate x86 assembly *)
open Ast

exception CodeGenError of string

let rec gen_program program = 
    let buf = Buffer.create 1024 in
    Buffer.add_string buf "    .att_syntax\n";
    Buffer.add_string buf "    .global main\n";
    Buffer.add_string buf "    .text\n\n";
    
    List.iter (gen_decl buf) program;
    Buffer.contents buf

and gen_decl buf = function
    | Function_decl (name, params, body) -> 
        Buffer.add_string buf (name ^ ":\n");
        Buffer.add_string buf "    push %rbp\n";
        Buffer.add_string buf "    mov %rsp, %rbp\n";
        (* reserve space for params *)
        if List.length params > 0 then
            Buffer.add_string buf ("    sub $" ^ string_of_int (8 * List.length params) ^ ", %rsp\n");

        (* allocate params to stack *)
        List.iteri (fun i param -> 
            let offset = -8 * (i + 1) in
            Buffer.add_string buf ("    mov %rdi, " ^ string_of_int offset ^ "(%rpb)\n")
        ) params;

        gen_stmt buf body;
        Buffer.add_string buf "    mov %rbp, %rsp\n";
        Buffer.add_string buf "    pop %rbp\n";
        Buffer.add_string buf "    ret\n\n"

and gen_stmt buf = function 
    | Return_stmt expr -> 
        gen_expr buf expr;
        Buffer.add_string buf "    mov %rax, %rbx\n";
        Buffer.add_string buf "    mov $60, %rax\n";
        Buffer.add_string buf "    syscall\n"
    | Expr_stmt expr -> gen_expr buf expr
    | Block_stmt stmts -> List.iter (gen_stmt buf) stmts
    | _ -> raise (CodeGenError "Unsupport statement")

and gen_expr buf = function
    | IntLiteral_expr i -> 
        Buffer.add_string buf ("    mov $" ^ string_of_int i ^ ", %rax\n")
    | Identifier_expr ident -> 
        Buffer.add_string buf ("    mov -8(%rbp), %rax\n")
    | BinOp_expr (op, left, right) ->
        gen_expr buf left;
        Buffer.add_string buf "    push %rax\n";
        gen_expr buf right;
        Buffer.add_string buf "    pop %rbx\n";
        (match op with
        | Add_op -> Buffer.add_string buf "    add %rbx, %rax\n"
        | Sub_op -> Buffer.add_string buf "    sub %rbx, %rax\n"
        | Mul_op -> Buffer.add_string buf "    imul %rbx, %rax\n"
        | Div_op -> Buffer.add_string buf "    idiv %rbx\n"
        | _ -> raise (CodeGenError "Unsupport operation"))
    | _ ->  raise (CodeGenError "Unsupport expression")

