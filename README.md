# Execution paths

## Testing

To run a test, uncomment the corresponding line in `test/scala/testexecpath/TestExecPath.scala`.

For instance, to test the first example on the inter-procedural building of execution paths, uncomment the following line:

```scala
testGetAllExecPaths("/home/ravil/IdeaProjects/execution-paths/data/InterProcedural1.java", "accessFile", "file", 55)
```

and run this command using `sbt`:

```scala
testOnly testexecpath.TestExecPath
```

## `CFGraphPathBuilder` object

1. To build an **execution path**, use the following method of the `CFGraphPathBuilder` object:

```scala
def generateAllExecPathsInterProcWithCrawlIn(nodeToExpand: CfgNode, upUntilNode: Option[CfgNode] = None, filterByDdg: Boolean = true, onlyCallNodes: Boolean = true, steps: Int = Int.MaxValue)(implicit cpg: Cpg): List[List[CfgNode]]
```

This method generates all execution paths (intra- and inter-procedurally) from the given starting node, optionally up to a specified node. The method traverses both within the method's scope and across method calls, and can further extend its traversal through the 'crawl-in' of the intermediate method calls. It also offers options to filter paths based on data dependency to the specified starting node and to only consider call nodes.

The method has the following parameters:

```scala
@param nodeToExpand  The starting node (sink) from which execution paths should be generated.
@param upUntilNode   Optional node to stop the traversal (source). If provided, paths are generated up to this node.
@param filterByDdg   Flag to decide whether to filter paths based on data dependency graph (DDG).
                     If `true`, only paths that are predecessors with respect to data dependency to
                     the sink node are considered. Default is `true`.
@param onlyCallNodes Flag to decide whether to only consider call nodes in the paths.
                     If `true`, paths are filtered to only include call nodes. Default is `true`.
@param steps         The maximum number of steps (or nodes) to traverse in the generated paths.
                     Default is `Int.MaxValue`, indicating no limit.
@param cpg           Code Property Graph (CPG) which implicitly provides necessary context for graph operations.
@return A list of execution paths. Each path is represented as a list of `CfgNode` elements.
```

Minimal working example (for the `InterProcedural1.java` test instance):

```scala
// First, obtain the CPG of the input source code, 
// which is contained in the variable `cpg`. Then:
val startNode = cpg.method("accessFile").cfgNode.code("file").lineNumber(55).head
val targetNode = cpg.method("processInput").cfgNode.code("position").lineNumber(17).head
val allExecPaths = generateAllExecPathsInterProcWithCrawlIn(startNode, targetNode, filterByDdg = true, onlyCallNodes = false)(cpg)
```

2. To build an **execution slice**, firstly, obtain the set of all nodes that depend on the starting node (sink) using the following method of the `CFGraphPathBuilder` object:

```scala
def traverseDdgBackwardsInterProcWithCrawlInUpUntilNode(startNode: CfgNode, upUntilNode: Option[CfgNode] = None)(implicit cpg: Cpg): MultiSet[CfgNode]
```

This method traverses the data dependency graph (DDG) backwards from a starting node, considering inter-procedural dependencies. The traversal also accounts for intermediate function calls and attempts to 'crawl' inside them when they are encountered. Optionally, the search stops when the target node is encountered.

Specifically, this function performs a depth-first search (DFS) on the DDG. When it encounters a function call, it will try to dive inside that function and continue the traversal from its return node. If the traversal within the function finishes and there are no preceding nodes, the traversal will attempt to move backwards inter-procedurally, either by resuming within the scope of the node that had the function call or by checking the callers of the current method.

The method has the following parameters:

```scala
@param startNode   The starting node from which the traversal begins.
@param upUntilNode Optional node to stop the traversal (source). If provided, result is generated up to this node.
@param cpg         The code property graph, an implicit parameter used for the traversal.
@return A multi-set of `CfgNode` which represents the nodes visited during the traversal.
```

Minimal working example (for the `InterProcedural1.java` test instance):

```scala
// First, obtain the CPG of the input source code, 
// which is contained in the variable `cpg`. Then:
val startNode = cpg.method("accessFile").cfgNode.code("file").lineNumber(55).head
val targetNode = cpg.method("processInput").cfgNode.code("position").lineNumber(17).head
val execSlice = traverseDdgBackwardsInterProcWithCrawlInUpUntilNode(startNode, targetNode)(cpg)
```

## Wrapping the result

To wrap the obtained result, which is an instance of `List[CfgNode]`, we can use the `main/utils/ExecPath2CodeExtractor` module. It has the following main method:

```scala
def extract(execPath: List[AstNode], inOrderOfList: Boolean = false)(implicit cpg: Cpg): List[String]
```

Given a list of `CfgNodes` `execPath`, this method extracts the code lines corresponding to the nodes contained in the list. The extraction can be performed according to the following two regimes:

- When the toggle `inOrderOfList` is `false`, the list nodes are mapped to their code lines, then the obtained code lines are deduplicated and arranged according to their order in the original source file;
- When the toggle `inOrderOfList` is `true`, the list nodes are mapped to their code lines, but the order of the nodes in the list are kept and respected. Consecutive duplicate code lines are handled.