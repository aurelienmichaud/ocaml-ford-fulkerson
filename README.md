# ocaml-ford-fulkerson

 - fordfulk: automated bash script to make, run ftest bytecode executable AND export the graphs into svg format

## src Directory

 - graph: Graphs implementation.

 - gfile: File handling to translate files into graph, and to write graph back into files.

 - flownetwork: Flow networks implementation, with BFS and DFS algorithms available. Basically a graph specialization.

 - weightedflownetwork: Weighted Flow networks implementation, with BFS, DFS and Bellman-Ford algorithms available. Basically a flow network with an additional weight label

 - bipartite: Specialization of weighted flow networks applied to bipartite problems.

 - ffile: File handling, using gfile, but for flow networks.

 - wffile: File handling for weighted flow networks.

 - bfile: File handling for bipartite graphs.

## graphs Directory

 - graph*: Graphs examples.
 - *.flwnt: Flow networks examples.
 - *.bprtt: Bipartite examples.
 - tosvg: Bash tool to translate all *.dot files in graph/ into their *.svg representation.

```bash
cd ./graphs && ./tosvg
```

	
## graphs/SVGs Directory

All svg files representing the graphs. You can then have a nice display of all the graphs by executing

```bash
xdg-open ./graph*.svg
```

or

```bash
firefox ./graph*.svg
```

for example.

## Flow networks file format

It's the same format as the graph file format, but the label can either be one number, in that case that number is considered to be the capacity and the flow is set to 0. Otherwise the label can be written as follow : 2/15, which will be translated as flow = 2 and capacity = 15.

In order for ftest to consider the file as a flow network solving problem you need to add the 'flwnt' extension to the file.

## Bipartite file format

Comments are written with a '%' at the beginning of the line.

Blanks are ignored.

In order for ftest to consider a bipartite file and algorithm you need to add the '.bprtt' extension to the file.

There are 3 types of patterns :

### Pattern 1

```
name1[, name2,...] = choiceI[, choiceJ,...]
```
This pattern means that 'name1', 'name2', etc will be linked to 'choiceI', 'choiceJ', etc.

Example :
```
Aurelien, Martin, Didier = Ocaml, Baseball
Martin = Tennis
```

Aurelien, Martin and Didier are each linked to Ocaml and Baseball and Martin is also linked to Tennis.

### Pattern 2

Same as pattern 1 but we can add the weight 'N' (integer) of the link. Keep in mind that the algorithm will consider smaller weights first for parsing the bipartite graph.

```
N|name1[, name2,...] = choiceI[, choiceJ, ...]
```

'name1', 'name2', etc will be linked to 'choiceI', 'choiceJ', etc with a weight of 'N'. By default, using the pattern 1 will set a weight of 1

Example :

```
3|Aurelien = Golf
```

Meaning that Aurelien is linked to Golf with a weight of 3.

### Pattern 3

This pattern is used to tell the capacity 'N' (integer) of a right-side node. By default, such a capacity is set to 1

```
choiceI:N
```

Example :

```
Ocaml:4
```

Meaning that Ocaml choice is accepting up to 4 users.

