open Ast

type parser_state = {
    tokens: Lexer.token list;
    mutable cur_pos: int;
}

let create_parser tokens = { tokens; cur_pos = 0 }

exception ParserError of string

let peek state = 
    if state.cur_pos < List.length state.tokens then
        match List.nth_opt state.tokens state.cur_pos with
        | Some t -> t
        | None -> raise (ParserError "Failed to peek: token list is too short")
    else Lexer.END

let advance state = 
    if state.cur_pos < List.length state.tokens then
        state.cur_pos <- state.cur_pos + 1
    else raise (ParserError "Failed to advance: current position is the end of token list")

let consume state token = 
    if peek state = token then advance state 
    else raise (ParserError ("Expected " ^ string_of_int state.cur_pos))


let rec parse state =  
    let rec parse_decls () =
    match peek state with
    | Lexer.INT -> 
        let decl = parse_decl state in
        decl :: parse_decls ()
    | _ -> []
    in
    parse_decls ()

(* parse declaration *)
(* todo: add variable *)
and parse_decl state =
    match peek state with
    | Lexer.INT-> parse_function state
    | _ -> raise (ParserError "Expected declaration")

(* parse function *)
and parse_function state =
    consume state INT;
    match peek state with 
    | Lexer.Identifier ident -> 
        advance state;
        consume state Lexer.LParen;
        let params = parse_params state in
        consume state Lexer.RParen;
        consume state Lexer.LBrace;
        let body = parse_stmt state in
        consume state Lexer.RBrace;
        Function_decl (ident, params, body)
    | _ -> raise (ParserError "Expected function name")

(* parse the params of the declaration of functions *)
and parse_params state = 
    match peek state with 
    | Lexer.INT ->  (* todo: change int to type *) 
       advance state;
       (match peek state with 
        | Lexer.Identifier name ->
            advance state;
            (match peek state with 
            | Lexer.Comma -> 
                advance state;
                name :: parse_params state
            | _ -> [name])
        | _ -> raise (ParserError "Expected parameter name"))
    | _ -> []

(* parse statement *)
and parse_stmt state = 
    match peek state with 
    | Lexer.RETURN -> 
        advance state;
        let expr = parse_expr state in
        consume state Lexer.Semicolon;
        Return_stmt expr
    | Lexer.IF ->
        advance state;
        consume state Lexer.LParen;
        let cond = parse_expr state in
        consume state Lexer.RParen;
        let then_branch = parse_stmt state in
        let else_branch = 
            match peek state with 
            | Lexer.ELSE -> 
                advance state;
                Some (parse_stmt state)
            | _ -> None
        in 
        If_stmt (cond, then_branch, else_branch)
    | Lexer.WHILE ->
        advance state;
        consume state Lexer.LParen;
        let cond = parse_expr state in 
        consume state Lexer.RParen;
        let body = parse_stmt state in
        While_stmt (cond, body)
    | Lexer.LBrace -> 
        advance state;
        let stmts = parse_block state in 
        consume state Lexer.RBrace;
        Block_stmt stmts
    | _ -> 
        let expr = parse_expr state in
        consume state Lexer.Semicolon;
        Expr_stmt expr

(* parse block *)
and parse_block state = 
    match peek state with
    | Lexer.RBrace -> []
    | _ -> 
        let stmt = parse_stmt state in
        stmt :: parse_block state

(* parse expression *)
and parse_expr state = parse_assign state

(* parse assign expression *)
and parse_assign state = 
    let left = parse_equality state in
    match peek state with 
    | Lexer.Equal -> 
        advance state;
        (match left with
        | Identifier_expr ident -> Assign_expr (ident, parse_assign state)
        | _ -> raise (ParserError "Invalid assignment target"))
    | _ -> left

(* parse equality expression *)
and parse_equality state = 
    let expr = parse_relational state in
    match peek state with 
    | Lexer.Eq -> 
        advance state;
        BinOp_expr (Eq_op, expr, parse_equality state)
    | Lexer.Ne -> 
        advance state;
        BinOp_expr (Ne_op, expr, parse_equality state)
    | _ -> expr

(* parse relational expression *)
and parse_relational state = 
    let expr = parse_additive state in
    match peek state with 
    | Lexer.Lt ->
        advance state;
        BinOp_expr (Lt_op, expr, parse_relational state)
    | Lexer.Gt ->
        advance state;
        BinOp_expr (Gt_op, expr, parse_relational state)
    | Lexer.Le ->
        advance state;
        BinOp_expr (Le_op, expr, parse_relational state)
    | Lexer.Ge ->
        advance state;
        BinOp_expr (Ge_op, expr, parse_relational state)
    | _ -> expr

(* parse add/sub operation *)
and parse_additive state =
    let expr = parse_multiplicative state in
    match peek state with
    | Lexer.Add -> 
        advance state;
        BinOp_expr (Add_op, expr, parse_additive state)
    | Lexer.Sub -> 
        advance state;
        BinOp_expr (Sub_op, expr, parse_additive state)
    | _ -> expr

(* parse multipal/divide operation *)
and parse_multiplicative state =
    let expr = parse_unary state in
    match peek state with
    | Lexer.Asterisk-> 
        advance state;
        BinOp_expr (Mul_op, expr, parse_multiplicative state)
    | Lexer.Slash -> 
        advance state;
        BinOp_expr (Div_op, expr, parse_multiplicative state)
    | _ -> expr

(* parse unary expression *)
and parse_unary state =
  match peek state with
  | Lexer.Sub ->
      advance state;
      UnaryOp_expr (Neg_op, parse_unary state)
  | Lexer.Not ->
      advance state;
      UnaryOp_expr (Not_op, parse_unary state)
  | _ -> parse_primary state

(* parse primary expression *)
and parse_primary state = 
    match peek state with 
    | Lexer.Integer n ->
        advance state;
        IntLiteral_expr n
    | Lexer.Identifier ident ->
        advance state; 
        (match peek state with 
        | Lexer.LParen ->
            advance state;
            let args = parse_args state in
            consume state Lexer.RParen;
            Call_expr (ident, args)
        | _ -> Identifier_expr ident)
    | Lexer.LParen ->
        advance state;
        let expr = parse_expr state in
        consume state Lexer.RParen;
        expr
    | _ -> raise (ParserError "Expected primary expression")

(* parse arguments for calling function *)
and parse_args state = 
    match peek state with 
    | Lexer.RParen -> []
    | _ -> 
        let arg = parse_expr state in 
        (match peek state with
        | Lexer.Comma -> 
            advance state;
            arg :: parse_args state
        | _ -> [arg])




