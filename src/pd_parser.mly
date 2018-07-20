%token <string> STRING
%token <float> FLOAT
%token <string> IDENT
%token <int> INT
%token SEMICOLON
%token SHARP
%token EOF
%token DASH
(* Chunk types*)
%token ARRAY
%token OBJECT
%token WINDOW
(* Element types *)
%token CONNECT
%token OBJ
%token TEXT
%token MESSAGE
%token FLOATATOM
%token RESTORE
%token OARRAY
%token COORDS

%token CANVAS

%{
open Puredata
module Array = BatArray
module Enum = BatEnum
module Option = BatOption
%}


%start <Puredata.patch > prog
%%

prog:
  |  l = list(data)  EOF
    { l }
  ;

data:
  |   SHARP; c = chunk; SEMICOLON
   { c}
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
  | i = INT; l = array_elements
    {Enum.push l (float_of_int i);l }
  | f = FLOAT; l = array_elements
    {Enum.push l f;l }
  ;

window_args:
  | CANVAS ; pos = position ; sz = size ; font_num = window_args2
    { MainWindow(pos, sz, font_num) }
  ;

window_args2:
  | font_num = INT {  font_num }
  | id = IDENT ; open_on_load = INT { 0}


object_args:
  | CONNECT ; source = INT ; inlet = INT ; sink = INT ; outlet = INT
    { Connect(source, inlet, sink, outlet) }
  | OBJ ; pos = position ; name = IDENT ; args = option(STRING)
    {Obj(pos, name, Option.default "" args)}
  | MESSAGE ; pos = position ; msg = STRING
    {Msg(pos, msg)}
  | TEXT ; pos = position ; text = STRING
    { Text(pos, text)}
  | FLOATATOM ; pos = position ; w = INT ; lower_limit = INT ; upper_limit = INT ; label_pos = option(INT) ; opt_atom_value opt_atom_value opt_atom_value
    { Floatatom(pos, w, lower_limit, upper_limit) }
  | RESTORE ; any = STRING
    { Any any  }
  | OARRAY ; any = STRING
    { Any any }
  | COORDS ; any = STRING
    { Any any }
  ;


chunk_prolog(name):
  | name; pos = position
      { pos}

size:
  | w = INT; h = INT
    { {w  ; h} }
  ;

position:
  | x = INT; y = INT
    { {x ; y} }
  ;

opt_atom_value:
  |   { None }
  | i = INT  {Some i }
  | DASH {None}
  ;

%%
