%token <string> STRING
%token <float> FLOAT
%token <string> IDENT
%token <int> INT
%token SEMICOLON
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
%token FLOATATOM
%token RESTORE
%token OARRAY
%token COORDS

%token CANVAS

%token POP
%token PD
%token GRAPH

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
  |  c = chunk; SEMICOLON
   { c}
   ;

chunk:
  | ARRAY ; ar = array_elements
    { Pdarray (Array.of_backwards ar) }
  | WINDOW ; w = window_args
    { Pdwindow w}
  | OBJECT ; obj = object_args
    { Pdobject obj }
  | OBJECT ; obj = graph_object_args
    {Pdwindow obj}
  ;

array_elements:
  | { Enum.empty () }
  | i = INT; l = array_elements
    {Enum.push l (float_of_int i);l }
  | f = FLOAT; l = array_elements
    {Enum.push l f;l }
  ;

window_name:
  | id = IDENT {id}
  | GRAPH {"graph"}
  ;

window_args:
  | CANVAS ; pos = position ; sz = size ; font_num = INT
    { MainWindow(pos, sz, font_num) }
  | CANVAS ; pos = position ; sz = size ; id = window_name ; open_on_load = INT
    { Subpatch(pos, sz, id) }
  ;


obj_ident:
  | n = INT {string_of_int n}
  | n = FLOAT {string_of_float n}
  | n = IDENT {n}
  ;

restore_kind:
  | PD {Pd}
  | GRAPH {Graph}
  ;

object_args:
  | CONNECT ; source = INT ; inlet = INT ; sink = INT ; outlet = INT
    { Connect(source, inlet, sink, outlet) }
  | OBJ ; pos = position ; name = obj_ident ; args = option(STRING)
    {Obj(pos, name, Option.default "" args)}
  | MESSAGE ; pos = position ; msg = option(STRING)
    {Msg(pos, Option.default "" msg)}
  | TEXT ; pos = position ; text = STRING
    { Text(pos,  text)}
  | FLOATATOM ; pos = position ; w = INT ; lower_limit = INT ; upper_limit = INT ; option(STRING)
    { Floatatom(pos, w, lower_limit, upper_limit) }
  | RESTORE ; pos = position ; t = restore_kind ; name = option(window_name)
    {Restore(pos, t, name) }
  | OARRAY ; any = STRING
    { Any any }
  | COORDS ; any = STRING
    { Any any }
  | POP
    {Restore (make_position (), Graph, None)}
  ;

graph_object_args:
  | GRAPH ; name = IDENT ; any = STRING
    {Subpatch(make_position (), make_size (), name)}
  ;

chunk_prolog(name):
  | name; pos = position
      { pos}
  ;

size:
  | w = INT; h = INT
    { {w  ; h} }
  ;

position:
  | x = INT; y = INT
    { {x ; y} }
  ;

(*opt_atom_value:
  |   { None }
  | i = INT  {Some i }
  | IDENT {None}
  | DASH {None}
  ;
  *)

%%
