%token <string> STRING
%token <float> FLOAT
%token <string> IDENT
%token <int> INT
%token SEMICOLON
%token COLON
%token DOT
%token EQUAL
%token EOF
%token LBRACE
%token RBRACE
%token ARROW
%token COMMA

(* Various distinguished attributes*)
%token INLETS
%token OUTLETS
%token TEXT
%token DEADLINE
%token KIND
%token WCET

%{
  open Flowgraph
  module Array = BatArray
  module Enum = BatEnum
  module Option = BatOption

%}

%start <Flowgraph.node list * Flowgraph.connection list * float option > prog
%%

prog:
  | l = statements EOF {l}
  ;

statements:
  | {([], [], None)}
  |  n = node SEMICOLON stat = statements {if Node.is_valid n then let (nodes, edges, deadline) = stat in (n :: nodes, edges, deadline) else failwith ("Invalid node: " ^(show_node n))}
  | e = edges SEMICOLON stat = statements {let (nodes, edges,  deadline) = stat in (nodes, e @ edges, deadline)}
  | d = deadline SEMICOLON stat = statements {let (nodes, edges,  _) = stat in (nodes, edges, d)}
  ;

deadline:
  | DEADLINE EQUAL duration = FLOAT  {Some duration}

node:
  | id = IDENT EQUAL LBRACE node = attributes RBRACE {
    {node with id}
  }

distinguished_attr(X,T):
  X COLON v = T { v}

attributes:
  | {Node.empty}
  | num = distinguished_attr(INLETS, INT) COMMA node = attributes { {node with nb_inlets=num}}
  | num = distinguished_attr(OUTLETS, INT) COMMA node = attributes { {node with nb_outlets=num}}
  | num = distinguished_attr(WCET, FLOAT) COMMA node = attributes { {node with wcet = Some num}}
  | text = distinguished_attr(TEXT, STRING) COMMA node = attributes { {node with text=Some text}}
  | kind = distinguished_attr(KIND, STRING) COMMA node = attributes { {node with className=kind}}
  | id = IDENT COLON v = STRING COMMA node = attributes { {node with more=(id,v)::node.more}}
  ;

edges:
  | s = IDENT DOT s_p = INT ARROW d = IDENT DOT d_p = INT {[{source_node = s ; source_port = s_p ; destination_node = d ; destination_port = d_p}]}
  | s = IDENT DOT s_p = INT ARROW e = edges
    {
      let last_node = List.hd e in
      {source_node = s ; source_port = s_p ; destination_node = last_node.source_node ; destination_port = last_node.source_port} :: e
    }
 ;



%%
