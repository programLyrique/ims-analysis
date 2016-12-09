type position = {x : int ; y : int}

type size = {w : int ; h : int}

type pdobject =
  | Connect of int * int * int * int
  | Obj of position * string * string
  | Msg of position * string
  | Text of position * string
  | Floatatom of position * int * int * int
  | Any of string

type pdwindow =
  | MainWindow of position * size * int

type patch = Pdarray of float array | Pdwindow of pdwindow | Pdobject of pdobject
