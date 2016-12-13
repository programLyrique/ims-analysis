type position = {x : int ; y : int}
[@@deriving show]

type size = {w : int ; h : int}
[@@deriving show]

type kind = Graph | Pd
[@@deriving show]

type pdobject =
  | Connect of int * int * int * int
  | Obj of position * string * string
  | Msg of position * string
  | Text of position * string
  | Floatatom of position * int * int * int
  | Any of string
[@@deriving show]

type pdwindow =
  | MainWindow of position * size * int
[@@deriving show]

type patch_statement = Pdarray of float array | Pdwindow of pdwindow | Pdobject of pdobject
[@@deriving show]


type patch = patch_statement list
[@@deriving show]
