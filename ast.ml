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
