{
open Batteries
open Lexing
open Pd_parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

let format_pos_error lexbuf =
  let pos = lexbuf.lex_curr_p in
    "line " ^ string_of_int pos.pos_lnum ^ " character " ^ string_of_int (pos.pos_cnum - pos.pos_bol + 1)


(* To parse the unquoted strings at the end of a command, before a ;
Unquoted_string n counts the number of tokens before we have to switch to the string lexer.
The string lexer uses ";" as a termination symbol and so we can generate SEMICOLON as a token in the string lexer
as sublexers in ocamllex can only generate one token. Nevertheless, we indicate in state that we scanned a string with
Previously_string and then, when finding a newline (which we always as after a semicolon), we emit a SEMICOLON token.
*)
type state = Commands | Previously_string | Unquoted_string of int

}

let int = '-'? ['0'-'9'] ['0'-'9']*

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = '-'? digit+ '.'? digit* exp?
let id = ['a'-'z' 'A'-'Z' '_' ] ['a'-'z' 'A'-'Z' '0'-'9' '_' ]*

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"


rule read state =
  parse
  | white    {
    (match !state with
      | Previously_string | Commands -> read
      | Unquoted_string n when n = 0 -> read_string (Buffer.create 17)
      | Unquoted_string n -> (state := Unquoted_string (n-1) ; read ) )
      state
      lexbuf
     }
  | newline  { next_line lexbuf;
      match !state with
        | Previously_string -> (
          state := Commands;
          SEMICOLON
          )
        | _ -> read state lexbuf }
  | int  as num    { INT (int_of_string num) }
  | float as num   { FLOAT (float_of_string num) }
  | ";"      { SEMICOLON }
  | "#A"      { state := Commands; ARRAY }
  | "#X"      { state := Commands ; OBJECT }
  | "#N"      { state := Commands; WINDOW }
  | "connect" { CONNECT }
  | "obj"    {state := Unquoted_string 3; OBJ }
  | "text"   { state := Unquoted_string 2; TEXT }
  | "msg"    {state := Unquoted_string 2; MESSAGE }
  | "canvas" { CANVAS }
  | "floatatom" { state := Unquoted_string 5;  FLOATATOM }
  | "restore"   { (*state := Unquoted_string 0 ;*) RESTORE }
  | "coords"    { state := Unquoted_string 0 ; COORDS }
  | "array"     { state := Unquoted_string 0 ; OARRAY }
  | "pd"        {PD}
  | "graph"     {state := Unquoted_string 1; GRAPH}
  | "pop"       {POP}
  | [^ ' ' '\t' '\n' '\r' ';']+ as ident    { IDENT ident }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf ^ "\t" ^ format_pos_error lexbuf  ^"\n") ) }
  | eof      { EOF }
and read_string buf state =
  parse
  | [^ ';' '\\']* as s { Buffer.add_string buf s ; read_string buf state lexbuf }
  | '\\' as s { Buffer.add_char buf s ; read_string buf state lexbuf }
  | "\\;" as s { Buffer.add_string buf s ; read_string buf state lexbuf }
  | "\\," as s { Buffer.add_string buf s ; read_string buf state lexbuf }
  | ';' {state := Previously_string ; STRING (Buffer.contents buf)}
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf ^  " " ^ format_pos_error lexbuf ^ "\n") ) }
  | eof { raise (SyntaxError ("String is not terminated by semicolon\n")) }

{
  (* Use deriving instead of this function. For that, we will need to define the type token precisely in the prolog. Or ppx_import? *)
  let token_to_string = function
    | INT i -> "INT[" ^ (string_of_int i) ^"]"
    | FLOAT f -> "FLOAT[" ^(string_of_float f) ^"]"
    | SEMICOLON -> "SEMICOLON"
    | ARRAY -> "\nARRAY"
    | OBJECT ->  "\nOBJECT"
    | WINDOW -> "\nWINDOW"
    | CONNECT  -> "CONNECT"
    | OBJ  -> "OBJ"
    | TEXT -> "TEXT"
    | MESSAGE ->  "MESSAGE"
    | RESTORE -> "RESTORE"
    | CANVAS -> "CANVAS"
    | FLOATATOM -> "FLOATATOM"
    | OARRAY -> "OARRAY"
    | COORDS -> "COORDS"
    | STRING s  -> "STRING[\"" ^ s ^"\"]"
    | IDENT s -> "IDENT[" ^s ^"]"
    | PD -> "PD"
    | POP -> "POP"
    | GRAPH -> "GRAPH"
    | EOF -> "EOF"


  let channel_to_tokens chan =
    try let lexbuf = Lexing.from_channel chan in
    let tok = ref WINDOW in
    let line = ref 0 in
    let state = ref Commands in
        while !tok != EOF do
          tok := read state lexbuf;
          let () = match !tok with
          |ARRAY | WINDOW | OBJECT ->
            begin
              incr line;
              Printf.printf "\n%d: " !line;
            end
          | _ -> () in
          Printf.printf "%s " (token_to_string !tok)
        done;
        print_newline ()
    with SyntaxError s -> Printf.fprintf stderr "Unexpected error while lexical analysis: %s" s;

}
