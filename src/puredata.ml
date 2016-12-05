type position = {x : int ; y : int}

type size = {w : int ; h : int}

type pdobject =
  | Connect of int * int * int * int
  | Obj of position * string * string
  | Msg of position * string
  | Any of string

type patch = Pdarray of float array | Pdwindow of string | Pdobject of pdobject