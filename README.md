# ocaml-ford-fulkerson

##src/ Directory

graph: Graphs implementation.
gfile: File handling to translate files into graph, and to write graph back into files.

flownetwork: Flow networks implementation. Basically a graph specialization.

ffile: File handling, using gfile, but for flow networks.

##graphs/ Directory

graph*: Graph examples.
tosvg: Bash tool to translate all *.dot files in graph/ into their *.svg representation.

```bash
cd ./graphs && ./tosvg
```

You can then have a nice display of all the graphs by executing

```bash
xdg-open ./graph/SVGs/graph*.svg
```

or

```bash
firefox ./graph/SVGs/graph*.svg
```

for example.
	
###SVGs/ Directory

All svg files representing the graphs.

