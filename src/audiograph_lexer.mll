(*
Lexer for common interchange format for audiographs
*)

{
  open Batteries
  open Lexing
  open Audiograph_parser

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

}


let int = '-'? ['0'-'9'] ['0'-'9']*

let digit = ['0'-'9']
let frac = '.' digit+
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit+ frac exp?
let ident = ['a'-'z' 'A'-'Z' '_' ] ['a'-'z' 'A'-'Z' '0'-'9' '_' ]*

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read =
  parse
  | "//"                    { c_comment lexbuf }
  | "/*"                    { block_comment lexbuf}
  | white {read lexbuf}
  | newline {next_line lexbuf; read lexbuf}
  | ";" {SEMICOLON}
  | "{" {LBRACE}
  | "}"  {RBRACE}
  | ":"  {COLON}
  | "." {DOT}
  | "," {COMMA}
  | "=" {EQUAL}
  | "->"  {ARROW}
  | int as num {INT (int_of_string num)}
  | float as num {FLOAT (float_of_string num)}
  | "in" {INLETS}
  | "out" {OUTLETS}
  | "text" {TEXT}
  | "kind" {KIND}
  | "wcet" {WCET}
  | ident as id {IDENT id}
  | '\"'                    {  double_quote_string (Buffer.create 20) lexbuf }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf ^ "\t" ^ format_pos_error lexbuf  ^"\n") ) }
  | eof {EOF}

and block_comment =
  parse
  | "*/"                    { read lexbuf }
  | newline                 {next_line lexbuf; block_comment lexbuf }
  | _                       { block_comment lexbuf }
  | eof    { raise (SyntaxError "Eof in comment") }

and c_comment =
  parse
  | newline                 { next_line lexbuf; read lexbuf }
  | _                       { block_comment lexbuf }
  | eof                     { raise (SyntaxError "Eof in comment") }

and double_quote_string buffer =
  parse
  | '"' { STRING (Buffer.contents buffer) }
  | "\\t" { Buffer.add_char buffer '\t'; double_quote_string buffer lexbuf }
  | "\\n" { Buffer.add_char buffer '\n'; double_quote_string buffer lexbuf }
  | "\\n" { Buffer.add_char buffer '\n'; double_quote_string buffer lexbuf }
  | '\\' '"' { Buffer.add_char buffer '"'; double_quote_string buffer lexbuf }
  | '\\' '\\' { Buffer.add_char buffer '\\'; double_quote_string buffer lexbuf }
  | eof { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf ^  " " ^ format_pos_error lexbuf ^ "\n") ) }
  | _ as char { Buffer.add_char buffer char; double_quote_string buffer lexbuf }

  {
    (* Pretty print*)


    (* type tok = [%import: Audiograph_parser.token] [@@deriving show] (* Does not work...*) *)

    module PrettyPrinter = struct
      type mytoken = Audiograph_parser.token =
        | WCET
        | TEXT
        | STRING of (string)
        | SEMICOLON
        | RBRACE
        | OUTLETS
        | LBRACE
        | KIND
        | INT of (int)
        | INLETS
        | IDENT of (string)
        | FLOAT of (float)
        | EQUAL
        | EOF
        | DOT
        | COMMA
        | COLON
        | ARROW
      [@@deriving show { with_path = false }]
    end

    let channel_to_tokens chan =
      try let lexbuf = Lexing.from_channel chan in
      let tok = ref SEMICOLON in
      let line = ref 0 in
      while !tok != EOF do
        tok := read lexbuf;
        if !tok = SEMICOLON then
        begin
          incr line;
          Printf.printf "\nLine %d: " !line;
        end;
        Printf.printf "%s " (PrettyPrinter.show_mytoken !tok)
      done;
      print_newline ()
      with SyntaxError s -> Printf.fprintf stderr "Unexpected error while lexical analysis: %s" s

    let print_position outx lexbuf =
      let pos = lexbuf.lex_curr_p in
      Printf.fprintf outx "%s:%d:%d" pos.pos_fname
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

    let parse_with_error_ag lexbuf =
      try prog read lexbuf with
      | SyntaxError msg ->
        Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg;
        exit(-1)
      | Error ->
        Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
        exit (-1)
  }
