
let read_file filename =
  try
    let ch = open_in_bin filename in
    let len = in_channel_length ch in
    let s = really_input_string ch len in
    close_in ch;
    s 
  with
  | Sys_error msg -> 
      Printf.eprintf "System error while reading %s: %s\n" filename msg;
      exit 1
  | End_of_file ->
      Printf.eprintf "Unexpected end of file: %s\n" filename;
      exit 1
  | Invalid_argument _ ->
      Printf.eprintf "Invalid argument (likely negative length) for file: %s\n" filename;
      exit 1


