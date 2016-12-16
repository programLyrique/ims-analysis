Interactive music system analysis
=================================



To do some code analysis on common IMS, such as Puredata or Max/MSP.


## Dependencies

- yojson
- batteries
- ocamlgraph
- ppx_deriving
- menhir

## Usage

``` bash
./main.native filename.[maxpat|pd]
```

Creates a `filename.ext.dot` file

To generate a png for instance, do:

``` bash
dot -Tpng -o filename2.png filename.ext.dot
```
