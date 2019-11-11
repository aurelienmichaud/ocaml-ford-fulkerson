# ocaml-ford-fulkerson

 - fordfulk: automated bash script to make, run ftest bytecode executable AND export the graphs into svg format

## src Directory

 - graph: Graphs implementation.

 - gfile: File handling to translate files into graph, and to write graph back into files.

 - flownetwork: Flow networks implementation. Basically a graph specialization.

 - ffile: File handling, using gfile, but for flow networks.

## graphs Directory

 - graph*: Graphs examples.
 - flownet*: Flow networks examples
 - tosvg: Bash tool to translate all *.dot files in graph/ into their *.svg representation.

```bash
cd ./graphs && ./tosvg
```

	
## graph/SVGs Directory

All svg files representing the graphs. You can then have a nice display of all the graphs by executing

```bash
xdg-open ./graph*.svg
```

or

```bash
firefox ./graph*.svg
```

for example.

