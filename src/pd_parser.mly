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
  ;

array_elements:
  | { Enum.empty () }
  | i = INT; l = array_elements
    {Enum.push l (float_of_int i);l }
  | f = FLOAT; l = array_elements
    {Enum.push l f;l }
  ;

window_args:
  | CANVAS ; pos = position ; sz = size ; font_num = INT
    { MainWindow(pos, sz, font_num) }
  | CANVAS ; pos = position ; sz = size ; id = IDENT ; open_on_load = INT
    { Subpatch(pos, sz, id) }
  ;


obj_ident:
  | n = INT {string_of_int n}
  | n = FLOAT {string_of_float n}
  | DASH {"-"} (*Substraction collides with the dash representing options*)
  | n = IDENT {n}

object_args:
  | CONNECT ; source = INT ; inlet = INT ; sink = INT ; outlet = INT
    { Connect(source, inlet, sink, outlet) }
  | OBJ ; pos = position ; name = obj_ident ; args = option(STRING)
    {Obj(pos, name, Option.default "" args)}
  | MESSAGE ; pos = position ; msg = STRING
    {Msg(pos, msg)}
  | TEXT ; pos = position ; text = STRING
    { Text(pos, text)}
  | FLOATATOM ; pos = position ; w = INT ; lower_limit = INT ; upper_limit = INT ; label_pos = option(INT) ; opt_atom_value opt_atom_value opt_atom_value
    { Floatatom(pos, w, lower_limit, upper_limit) }
  | RESTORE ; pos = position ; t = IDENT ; name = option(IDENT)
    {
      let t = match t with
      | "pd" -> Pd
      | "graph" -> Graph
      | ty -> failwith (Printf.sprintf "Unrecognized type %s for restore " ty) in
      Restore(pos, t, name)
    }
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
  | IDENT {None}
  | DASH {None}
  ;

%%
