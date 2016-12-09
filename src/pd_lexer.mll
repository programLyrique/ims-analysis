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

(* To parse the unquoted strings at the end of a command, before a ; *)
type state = Commands | Unquoted_string of int (*count number of white spaces *)

}

let int = '-'? ['0'-'9'] ['0'-'9']*

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?


let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"


rule read state =
  parse
  | white    {
    (match !state with
      | Commands -> read
      | Unquoted_string n when n = 0 -> read_string (Buffer.create 17)
      | Unquoted_string n -> (state := Unquoted_string (n-1) ; read ) )
      state
      lexbuf
     }
  | newline  { next_line lexbuf; read state lexbuf }
  | int  as num    { INT (int_of_string num) }
  | float as num    { FLOAT (float_of_string num) }
  | ";"      { SEMICOLON }
  | "#"      { SHARP }
  | "A"      { ARRAY }
  | "X"      { OBJECT }
  | "N"      { WINDOW }
  | "connect" { CONNECT }
  | "obj"    {state := Unquoted_string 2; OBJ }
  | "text"   { state := Unquoted_string 2; TEXT }
  | "msg"    {state := Unquoted_string 2; MESSAGE }
  | "canvas" { CANVAS }
  | "floatatom" { FLOATATOM }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf ^"\n") ) }
  | eof      { EOF }
and read_string buf state =
  parse
  | [^ ';' '\\']* as s { Buffer.add_string buf s ; read_string buf state lexbuf }
  | "\\;" as s { Buffer.add_string buf s ; read_string buf state lexbuf }
  | "\\," as s { Buffer.add_string buf s ; read_string buf state lexbuf }
  | ';' {state := Commands ; STRING (Buffer.contents buf)}
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf ^"\n") ) }
  | eof { raise (SyntaxError ("String is not terminated by semicolon\n")) }

{
  (* Use deriving instead of this function. For that, we will need to define the type token precisely in the prolog. *)
  let token_to_string = function
    | INT i -> "INT " ^ (string_of_int i)
    | FLOAT f -> "FLOAT " ^(string_of_float f)
    | SEMICOLON -> "SEMICOLON"
    | SHARP  -> "SHARP"
    | ARRAY -> "ARRAY"
    | OBJECT ->  "OBJECT"
    | WINDOW -> "WINDOW"
    | CONNECT  -> "CONNECT"
    | OBJ  -> "OBJ"
    | TEXT -> "TEXT"
    | MESSAGE ->  "MESSAGE"
    | CANVAS -> "CANVAS"
    | FLOATATOM -> "FLOATATOM"
    | STRING s  -> "STRING " ^ s
    | EOF -> "EOF"


  let channel_to_tokens chan =
    try let lexbuf = Lexing.from_channel chan in
    let tok = ref SHARP in
    let state = ref Commands in
        while !tok != EOF do
          tok := read state lexbuf;
          print_endline (token_to_string !tok)
        done
    with SyntaxError s -> Printf.fprintf stderr "Unexpected error while lexical analysis: %s" s;

}
