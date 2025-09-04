type token = Identifier of string 
            | Integer of int
            | Equal | Semicolon | Add | Sub | Asterisk | Slash |END 
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


let scan (input : string) (pos : int) : token list =
    if pos >= String.length input then END
    else 
        match input.[pos] with 
        | ' ' | '\t' | '\n' | '\r' -> scan input (pos + 1)
        | c when '0' <= c && c <= '9' -> handle_number input pos
        | c when 'a' <= c && c <= 'z' || 'A' <= c && 'Z' <= c || c = '_' -> handle_ident input pos
        | '=' -> Equal :: scan input (pos + 1)
        | ';' -> Semicolon :: scan input (pos + 1)
        | '+' -> Add :: scan input (pos + 1)
        | '-' -> Sub :: scan input (pos + 1)
        | '*' -> Asterick :: scan input (pos + 1)
        | '/' -> Slash :: input (pos + 1)
        | c -> raise (LexerError ("Unexpected character: " ^ String.of_char c))
    
and handle_number input pos = 
    (* get the last position of the input number *)
    let rec num_forward pos = 
        if pos < String.length input && '0' <= input.[pos] && input.[pos] <= '9'
        then handle_number input (pos + 1)
        else pos
    in 
    let last_pos = num_forward pos in
    let num_str = String.sub input (last_pos - pos) in
    Integer (int_of_string num_str) :: scan input end_pos

and handle_ident input pos = 
    (* get the last position of the input identifier *)
    let rec ident_forward pos = 
        if pos < String.length input then
            match input.[pos] with 
            | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> ident_forward input (pos + 1)
            | _ -> pos
        else pos
    in 
    let last_pos = ident_forward pos in
    let ident_str = String.sub input (last_pos - pos) in 
    try List.assoc ident keywords :: scan input last_pos
    with Not_Found Identifier :: scan input last_pos


            

    
    
