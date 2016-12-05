%token <string> STRING
%token <float> FLOAT
%token <int> INT
%token SEMICOLON
%token NEWLINE
%token SHARP
%token EOF
(* Chunk types*)
%token ARRAY
%token OBJECT
%token WINDOW
(* Element types *)
%token CONNECT
%token OBJ
%token TEXT
%token MESSAGE

%{
open Puredata
module Array = BatArray
module Enum = BatEnum
%}


%start <Puredata.patch option> prog
%%

prog:
  | EOF { None }
  | v = data {Some v}
  ;

data:
  | SHARP; c = chunk; SEMICOLON NEWLINE
   { c }
   ;

chunk:
  | ARRAY ; ar = array_elements
    { Pdarray (Array.of_backwards ar) }
  | WINDOW ; w = window_args
    { Pdwindow w}
  | OBJECT ; obj = object_args
    { Pdobject obj }
  ;

array_elements:
  | { Enum.empty () }
  | f = FLOAT; l = array_elements
    {Enum.push l f;l }
  ;

window_args:
  | s = STRING
    { s }
  ;


object_args:
  | CONNECT ; source = INT ; inlet = INT ; sink = INT ; outlet = INT
    { Connect(source, inlet, sink, outlet) }
  | OBJ ; pos = position ; name = STRING ; args = STRING
    {Obj(pos, name, args)}
  | MESSAGE ; pos = position ; msg = STRING
    {Msg(pos, msg)}
  | any = STRING (* We don't parse everything semantically, but we don't want to throw an error*)
    {Any any}
  ;


size:
  | w = INT; h = INT
    { {w  ; h} }
  ;

position:
  | x = INT; y = INT
    { {x ; y} }
  ;

%%
