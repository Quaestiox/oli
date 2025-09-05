type identifier = string

type binop = | Add_op | Sub_op | Mul_op | Div_op | Eq_op | Ne_op | Lt_op | Gt_op | Le_op | Ge_op

type unaryop = 
    | Neg_op | Not_op

type expr =
    | IntLiteral_expr of int
    | Identifier_expr of identifier
    | BinOp_expr of binop * expr * expr
    | UnaryOp_expr of unaryop * expr
    | Assign_expr of identifier * expr
    | Call_expr of identifier * expr list

type stmt =
    | Return_stmt of expr
    | Expr_stmt of expr
    | If_stmt of expr * stmt * stmt option
    | While_stmt of expr * stmt
    | Block_stmt of stmt list

type decl =
    | Function_decl of identifier * identifier list * stmt

type program = decl list

let rec string_of_expr = function
  | IntLiteral_expr n -> "IntLiteral(" ^ string_of_int n ^ ")"
  | Identifier_expr id -> "Identifier(\"" ^ id ^ "\")"
  | BinOp_expr (op, left, right) ->
      let op_str = match op with
        | Add_op -> "Add" | Sub_op -> "Sub" | Mul_op -> "Mul" | Div_op -> "Div"
        | Eq_op -> "Eq" | Ne_op -> "Ne" | Lt_op -> "Lt" | Gt_op -> "Gt" | Le_op -> "Le" | Ge_op -> "Ge"
      in
      "BinOp(" ^ op_str ^ ", " ^ string_of_expr left ^ ", " ^ string_of_expr right ^ ")"
  | UnaryOp_expr (op, expr) ->
      let op_str = match op with Neg_op -> "Neg" | Not_op -> "Not" in
      "UnaryOp(" ^ op_str ^ ", " ^ string_of_expr expr ^ ")"
  | Assign_expr (id, expr) -> "Assign(\"" ^ id ^ "\", " ^ string_of_expr expr ^ ")"
  | Call_expr (name, args) -> 
      "Call(\"" ^ name ^ "\", [" ^ String.concat "; " (List.map string_of_expr args) ^ "])"

let rec string_of_stmt = function
    | Return_stmt expr -> "Return(" ^ string_of_expr expr ^ ")"
    | Expr_stmt expr -> "Expr(" ^ string_of_expr expr ^ ")"
    | If_stmt (cond, then_branch, else_branch) ->
        let else_str = match else_branch with
          | None -> "None"
          | Some stmt -> "Some(" ^ string_of_stmt stmt ^ ")"
        in
        "If(" ^ string_of_expr cond ^ ", " ^ string_of_stmt then_branch ^ ", " ^ else_str ^ ")"
    | While_stmt (cond, body) -> "While(" ^ string_of_expr cond ^ ", " ^ string_of_stmt body ^ ")"
    | Block_stmt stmts -> "Block([" ^ String.concat "; " (List.map string_of_stmt stmts) ^ "])"
  
let string_of_decl = function
    | Function_decl (name, params, body) ->
        "Function(\"" ^ name ^ "\", [" ^ String.concat "; " params ^ "], " ^ string_of_stmt body ^ ")"

let print_ast ast = 
    print_endline("===== AST =====");
    List.iter (fun decl -> print_endline (string_of_decl decl)) ast
