%token <string> STRING
%token <float> FLOAT
%token <int> INT
%token SEMICOLON
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

%token CANVAS

%token FLOATATOM

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
  | SHARP; c = chunk; SEMICOLON
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
  | CANVAS ; pos = position ; sz = size ; font_num = INT
    { MainWindow(pos, sz, font_num) }
  ;


object_args:
  | CONNECT ; source = INT ; inlet = INT ; sink = INT ; outlet = INT
    { Connect(source, inlet, sink, outlet) }
  | OBJ ; pos = position ; name = STRING ; args = STRING
    {Obj(pos, name, args)}
  | MESSAGE ; pos = position ; msg = STRING
    {Msg(pos, msg)}
  | TEXT ; pos = position ; text = STRING
    { Text(pos, text)}
  | FLOATATOM ; pos = position ; w = INT ; lower_limit = INT ; upper_limit = INT
    { Floatatom(pos, w, lower_limit, upper_limit) }
  | any = STRING (* We don't parse everything semantically, but we don't want to throw an error *)
    {Any any}
  ;

chunk_prolog(name):
  | name; pos = position
      {(* Here return pos but alsop perform a side effect to tell that we are going to go into text mode until the semicolon *) pos}
      (* Add also an extended version. Or maybe an empty rule that just perform the side effect? *)

size:
  | w = INT; h = INT
    { {w  ; h} }
  ;

position:
  | x = INT; y = INT
    { {x ; y} }
  ;

%%
