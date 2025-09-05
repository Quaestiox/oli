type token = Identifier of string 
            | Integer of int
            | Equal | Semicolon | Add | Sub | Asterisk | Slash | END 
            | LParen | RParen | LBrace | RBrace | Comma | Eq | Ne | Lt | Gt | Le | Ge | Not
            (* All the keywords here are represented in uppercase *)
            | RETURN | IF | ELSE | WHILE | INT 

exception LexerError of string

let keywords = [
    ("int", INT);
    ("return", RETURN);
    ("if", IF);
    ("else", ELSE);
    ("while", WHILE);
]


let rec scan (input : string) (pos : int) : token list =
    if pos >= String.length input then [END]
    else 
        match input.[pos] with 
        | ' ' | '\t' | '\n' | '\r' -> scan input (pos + 1)
        | c when '0' <= c && c <= '9' -> handle_number input pos
        | c when 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z' || c = '_' -> handle_ident input pos
        | '=' when pos + 1 < String.length input && input.[pos + 1] = '=' -> Eq :: scan input (pos + 2)
        | '=' -> Equal :: scan input (pos + 1)
        | '!' when pos + 1 < String.length input && input.[pos + 1] = '=' -> Ne :: scan input (pos + 2)
        | '<' when pos + 1 < String.length input && input.[pos + 1] = '=' -> Le :: scan input (pos + 2)
        | '>' when pos + 1 < String.length input && input.[pos + 1] = '=' -> Ge :: scan input (pos + 2)
        | '<' -> Lt :: scan input (pos + 1)
        | '>' -> Gt :: scan input (pos + 1)
        | '!' -> Not :: scan input (pos + 1)
        | ';' -> Semicolon :: scan input (pos + 1)
        | '+' -> Add :: scan input (pos + 1)
        | '-' -> Sub :: scan input (pos + 1)
        | '*' -> Asterisk :: scan input (pos + 1)
        | '/' -> Slash :: scan input (pos + 1)
        | '(' -> LParen :: scan input (pos + 1)
        | ')' -> RParen :: scan input (pos + 1)
        | '{' -> LBrace :: scan input (pos + 1)
        | '}' -> RBrace :: scan input (pos + 1)
        | ',' -> Comma :: scan input (pos + 1)
        | c -> raise (LexerError ("Unexpected character: " ^ String.make 1 c))
    
and handle_number input pos = 
    let rec num_forward current_pos = 
        if current_pos >= String.length input then current_pos
        else if '0' <= input.[current_pos] && input.[current_pos] <= '9'
        then num_forward (current_pos + 1)
        else current_pos 
    in 
    let last_pos = num_forward pos in
    let num_str = String.sub input pos (last_pos - pos) in
    try
        Integer (int_of_string num_str) :: scan input last_pos
    with Failure _ ->
        raise (LexerError ("Invalid number: " ^ num_str))

and handle_ident input pos = 
    (* get the last position of the input identifier *)
    let rec ident_forward current_pos = 
        if current_pos >= String.length input then current_pos
        else
            match input.[current_pos] with 
            | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> ident_forward (current_pos + 1)
            | _ -> current_pos
    in 
    let last_pos = ident_forward pos in
    if last_pos = pos then
        raise (LexerError ("Invalid identifier starting at " ^ String.make 1 input.[pos]))
    else
        let ident_str = String.sub input pos (last_pos - pos) in
        match List.assoc_opt ident_str keywords with 
        | Some ident -> ident :: scan input last_pos
        | None -> Identifier ident_str :: scan input last_pos

let print_tokens (tokens : token list) = 
    print_endline("===== Tokens =====");
    let rec print_token_rec = function 
    | [] -> ()
    | token :: list -> 
        let token_str = match token with 
            | Identifier ident -> "<Identifier, " ^ ident ^ ">"
            | Integer i -> "<Integer, " ^ string_of_int i ^ ">"
            | Equal -> "<Equal>"
            | Semicolon -> "<Semicolon>"
            | Add -> "<Add>"
            | Sub -> "<Sub>"
            | Asterisk -> "<Asterisk>"
            | Slash -> "<Slash>"
            | LParen -> "<LParen>"
            | RParen -> "<RParen>"
            | LBrace -> "<LBrace>"
            | RBrace -> "<RBrace>"
            | Comma -> "<Comma>"
            | Eq -> "<Eq>"
            | Ne -> "<Ne>" 
            | Lt -> "<Lt>"
            | Le -> "<Le>"
            | Gt -> "<Gt>"
            | Ge -> "<Ge>"
            | Not -> "<Not>"
            | RETURN -> "<RETURN>"
            | IF -> "<IF>"
            | ELSE -> "<ELSE>"
            | WHILE -> "<WHILE>"
            | INT -> "<INT>"
            | END -> "<END>"
            | c -> raise (LexerError ("Unexpected token"))

        in 
        print_endline token_str;
        print_token_rec list
    in 
    print_token_rec tokens


           

    

