# ocaml-ford-fulkerson

src/ Directory

	graph: Graphs implementation.
	gfile: File handling to translate files into graph, and to write graph back into files.
	
	flownetwork: Flow networks implementation. Basically a graph specialization.

	ffile: File handling, using gfile, but for flow networks.

graph/ Directory

	graph*: Graph examples.
	tosvg: Bash tool to translate all *.dot files in graph/ into their *.svg representation.
	
	SVGs/ Directory

		All svg files representing the graphs.

