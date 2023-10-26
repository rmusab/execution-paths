package execpathbuilder

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.joern.dataflowengineoss.language.{ExtendedCfgNode, toExtendedCfgNode}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language.*

import utils.ExecPath2CodeExtractor

object CFGraphPathBuilder {

  // Immutable MultiSet implementation.
  // Time complexity:
  // `add` is O(log(n))
  // `count` is O(1) on average
  // `remove` is O(log(n))
  object MultiSet {
    def empty[T]: MultiSet[T] = new MultiSet(Map.empty[T, Int])

    def fromSet[T](set: Set[T]): MultiSet[T] = {
      val counts = set.map(e => (e, 1)).toMap
      new MultiSet(counts)
    }
  }
  class MultiSet[T](private val counts: Map[T, Int]) {

    def add(element: T): MultiSet[T] = {
      val newCount = counts.getOrElse(element, 0) + 1
      new MultiSet(counts + (element -> newCount))
    }

    def count(element: T): Int = counts.getOrElse(element, 0)

    def contains(element: T): Boolean = counts.contains(element)

    def remove(element: T): MultiSet[T] = {
      counts.get(element) match {
        case Some(c) if c > 1 => new MultiSet(counts + (element -> (c - 1)))
        case Some(_) => new MultiSet(counts - element)
        case None => this
      }
    }

    def toSet: Set[T] = counts.keySet
  }

  /**
   * A specialized version of `ExecPath2CodeExtractor` that utilizes a provided source string for extraction.
   *
   * This class overrides the default file reading behavior of `ExecPath2CodeExtractor` to use the provided source string
   * directly instead of reading from a file. This can be useful in scenarios where the source code is already available
   * in-memory and there's no need to access the filesystem for reading it.
   *
   * @param source The source code string that will be used for extraction.
   *
   * @example
   * {{{
   * val code = "int main() { return 0; }"
   * val extractor = new MyExecPath2CodeExtractor(code)
   * val result = extractor.extract(execPath)
   * }}}
   */
  class MyExecPath2CodeExtractor(source: String) extends ExecPath2CodeExtractor {
    override protected def readFile(filename: String): String = {
      source
    }
  }

  /**
   * Traverse the data dependency graph (DDG) backwards from a starting node, considering only intra-procedural dependencies.
   *
   * This function conducts a breadth-first traversal of the DDG. It begins at the provided node and progresses by exploring
   * nodes that are direct data-dependencies of the current node. Unlike the inter-procedural version, this function does not
   * transition between different methods but confines the traversal to nodes within the same procedure.
   *
   * To prevent revisiting, the traversal ensures that each node is visited only once.
   *
   * @param startNode The starting node from which the traversal begins.
   * @param cpg       The code property graph, an implicit parameter used for the traversal.
   * @return A set of `CfgNode` which represents the nodes visited during the traversal.
   */
  def traverseDdgBackwards(startNode: CfgNode)(implicit cpg: Cpg): Set[CfgNode] = {
    var visited = Set[CfgNode]()
    var queue = List[CfgNode](startNode)

    while (queue.nonEmpty) {
      val currentNode = queue.head
      queue = queue.tail

      if (!visited.contains(currentNode)) {
        visited += currentNode
        // Find the predecessors of the current node
        val predecessors = toExtendedCfgNode(currentNode).ddgIn.l
        queue ++= predecessors
      }
    }

    visited
  }

  /**
   * Traverse the data dependency graph (DDG) backwards from a starting node, considering only inter-procedural dependencies.
   *
   * This function performs a breadth-first traversal of the DDG. Starting from the provided node, it explores nodes that are
   * data-dependencies of the current node. If a node has no preceding data dependencies, the function attempts to move
   * backwards inter-procedurally by considering callers of the current method as potential predecessors.
   *
   * The traversal ensures that each node is visited only once.
   *
   * @param startNode The starting node from which the traversal begins.
   * @param cpg       The code property graph, an implicit parameter used for the traversal.
   * @return A set of `CfgNode` which represents the nodes visited during the traversal.
   */
  def traverseDdgBackwardsInterProc(startNode: CfgNode)(implicit cpg: Cpg): Set[CfgNode] = {
    var visited = Set[CfgNode]()
    var queue = List[CfgNode](startNode)

    while (queue.nonEmpty) {
      val currentNode = queue.head
      queue = queue.tail

      if (!visited.contains(currentNode)) {
        visited += currentNode
        // Find the predecessors of the current node
        var predecessors = toExtendedCfgNode(currentNode).ddgIn.l
        // If there are no preceding nodes to the current one,
        // try to go backwards inter-procedurally
        if (predecessors.isEmpty) {
          predecessors = currentNode.method.callIn.dedup.map(_.asInstanceOf[CfgNode]).l
        }
        queue ++= predecessors
      }
    }

    visited
  }

  /**
   * Traverse the data dependency graph (DDG) backwards from a starting node, considering inter-procedural dependencies.
   * The traversal also accounts for function calls and attempts to 'crawl' inside them when they are encountered.
   *
   * Specifically, this function performs a depth-first search (DFS) on the DDG. When it encounters a function call,
   * it will try to dive inside that function and continue the traversal from its return node.
   * If the traversal within the function finishes and there are no preceding nodes, the traversal will attempt
   * to move backwards inter-procedurally, either by resuming from the node that had the function call or by checking
   * the callers of the current method.
   *
   * Note: The traversal stops further exploring a specific node once it has been visited three times. This is to avoid
   * getting stuck in potential cycles.
   *
   * @param startNode The starting node from which the traversal begins.
   * @param cpg       The code property graph, an implicit parameter used for the traversal.
   * @return A multi-set of `CfgNode` which represents the nodes visited during the traversal.
   */
  def traverseDdgBackwardsInterProcWithCrawlIn(startNode: CfgNode)(implicit cpg: Cpg): MultiSet[CfgNode] = {
    @scala.annotation.tailrec
    def dfs(stack: List[(CfgNode, List[CfgNode])], visited: MultiSet[CfgNode]): MultiSet[CfgNode] = {
      stack match {
        case Nil => visited
        case (currentNode, callInStack) :: rest =>
          if (visited.count(currentNode) >= 3) {
            dfs(rest, visited)
          } else {
            val updatedVisited = visited.add(currentNode)
            // Find the predecessors of the current node
            var newCallInStack = callInStack
            var predecessors: List[CfgNode] =
              if (currentNode.isCall
                && currentNode.asInstanceOf[Call].callOut.nonEmpty
                && !currentNode.asInstanceOf[Call].callOut.head.cfgNode.exists(visited.contains)
                && !currentNode.asInstanceOf[Call].callOut.head.isExternal
                && currentNode.asInstanceOf[Call].callOut.head.cfgNode.exists(_.isReturn)) { // to handle: what if there are multiple signatures?
                val calleeMethod = currentNode.asInstanceOf[Call].callOut.head
                val calleeReturnNode = calleeMethod.cfgNode.filter(_.isReturn).head // to handle: what if there are multiple return statements in the callee method?
                newCallInStack = currentNode +: newCallInStack
                List(calleeReturnNode)
              } else toExtendedCfgNode(currentNode).ddgIn.l
            // If there are no preceding nodes to the current one,
            // try to go backwards inter-procedurally
            if (predecessors.isEmpty) {
              if (callInStack.nonEmpty) {
                predecessors = List(newCallInStack.head)
                newCallInStack = newCallInStack.tail
              } else {
                predecessors = currentNode.method.callIn.dedup.map(_.asInstanceOf[CfgNode]).l
              }
            }
            val newNodesToExpand = predecessors.map(bn => (bn, newCallInStack))
            dfs(rest ::: newNodesToExpand, updatedVisited)
          }
      }
    }

    dfs(List((startNode, List())), MultiSet.empty[CfgNode])
  }

  /**
   * Traverse the data dependency graph (DDG) backwards from a starting node, considering inter-procedural dependencies.
   * The traversal also accounts for intermediate function calls and attempts to 'crawl' inside them when they are
   * encountered. Optionally, the search stops when the target node is encountered.
   *
   * Specifically, this function performs a depth-first search (DFS) on the DDG. When it encounters a function call,
   * it will try to dive inside that function and continue the traversal from its return node.
   * If the traversal within the function finishes and there are no preceding nodes, the traversal will attempt
   * to move backwards inter-procedurally, either by resuming within the scope of the node that had the function call
   * or by checking the callers of the current method.
   *
   * Note: The traversal stops further exploring a specific node once it has been visited three times. This is to avoid
   * getting stuck in potential cycles.
   *
   * @param startNode   The starting node from which the traversal begins.
   * @param upUntilNode Optional node to stop the traversal (source). If provided, result is generated up to this node.
   * @param cpg         The code property graph, an implicit parameter used for the traversal.
   * @return A multi-set of `CfgNode` which represents the nodes visited during the traversal.
   */
  def traverseDdgBackwardsInterProcWithCrawlInUpUntilNode(startNode: CfgNode, upUntilNode: Option[CfgNode] = None)(implicit cpg: Cpg): MultiSet[CfgNode] = {
    @scala.annotation.tailrec
    def dfs(stack: List[(CfgNode, List[CfgNode])], visited: MultiSet[CfgNode]): MultiSet[CfgNode] = {
      stack match {
        case Nil => visited
        case (currentNode, callInStack) :: rest =>
          // Check if currentNode is the same as upUntilNode or upUntilNode is None
          val reachedUpUntilNode = upUntilNode.exists(_ == currentNode)
          if (visited.count(currentNode) >= 3) {
            dfs(rest, visited)
          } else if (reachedUpUntilNode) {
            val updatedVisited = visited.add(currentNode)
            dfs(rest, updatedVisited) // if reached target node, add the extended path to acc
          } else {
            val updatedVisited = visited.add(currentNode)
            // Find the predecessors of the current node
            var newCallInStack = callInStack
            var predecessors: List[CfgNode] =
              if (currentNode.isCall
                && currentNode.asInstanceOf[Call].callOut.nonEmpty
                && !currentNode.asInstanceOf[Call].callOut.head.cfgNode.exists(visited.contains)
                && !currentNode.asInstanceOf[Call].callOut.head.isExternal
                && currentNode.asInstanceOf[Call].callOut.head.cfgNode.exists(_.isReturn)) { // to handle: what if there are multiple signatures?
                val calleeMethod = currentNode.asInstanceOf[Call].callOut.head
                val calleeReturnNode = calleeMethod.cfgNode.filter(_.isReturn).head // to handle: what if there are multiple return statements in the callee method?
                newCallInStack = currentNode +: newCallInStack
                List(calleeReturnNode)
              } else toExtendedCfgNode(currentNode).ddgIn.l
            // If there are no preceding nodes to the current one,
            // try to go backwards inter-procedurally
            if (predecessors.isEmpty) {
              if (callInStack.nonEmpty) {
                predecessors = List(newCallInStack.head)
                newCallInStack = newCallInStack.tail
              } else {
                predecessors = currentNode.method.callIn.dedup.map(_.asInstanceOf[CfgNode]).l
              }
            }
            val newNodesToExpand = predecessors.map(bn => (bn, newCallInStack))
            dfs(rest ::: newNodesToExpand, updatedVisited)
          }
      }
    }

    dfs(List((startNode, List())), MultiSet.empty[CfgNode])
  }

  /**
   * Generates all execution paths (intra- and inter-procedurally) from the given starting node,
   * optionally up to a specified node.
   *
   * The method traverses both within the method's scope and across method calls, and can further
   * extend its traversal through the 'crawl-in' of the intermediate method calls. It also offers
   * options to filter paths based on data dependency to the specified starting node and to only
   * consider call nodes.
   *
   * @param nodeToExpand  The starting node (sink) from which execution paths should be generated.
   * @param upUntilNode   Optional node to stop the traversal (source). If provided, paths are generated up to this node.
   * @param filterByDdg   Flag to decide whether to filter paths based on data dependency graph (DDG).
   *                      If `true`, only paths that are predecessors with respect to data dependency to
   *                      the sink node are considered. Default is `true`.
   * @param onlyCallNodes Flag to decide whether to only consider call nodes in the paths.
   *                      If `true`, paths are filtered to only include call nodes. Default is `true`.
   * @param steps         The maximum number of steps (or nodes) to traverse in the generated paths.
   *                      Default is `Int.MaxValue`, indicating no limit.
   * @param cpg           Code Property Graph (CPG) which implicitly provides necessary context for graph operations.
   * @return A list of execution paths. Each path is represented as a list of `CfgNode` elements.
   * @example
   * {{{
   *   val paths = generateAllExecPathsInterProcWithCrawlIn(startNode, Some(targetNode), filterByDdg = false)
   * }}}
   *
   * Note:
   * The method uses a recursive helper function `iterate` that employs tail recursion to ensure
   * efficient traversal without stack overflows.
   */
  def generateAllExecPathsInterProcWithCrawlIn(nodeToExpand: CfgNode, upUntilNode: Option[CfgNode] = None, filterByDdg: Boolean = true, onlyCallNodes: Boolean = true, steps: Int = Int.MaxValue)(implicit cpg: Cpg): List[List[CfgNode]] = {
    // Obtain the set of all ddg-preceding nodes to the given one
    val ddgPrevNodes =
      if (filterByDdg) traverseDdgBackwardsInterProcWithCrawlIn(nodeToExpand)
      else MultiSet.empty[CfgNode]

    // nodesToExpand: (currentNode: CfgNode, currentPath: List[CfgNode], visitedNodes: MultiSet[CfgNode], callInStack: List[CfgNode])
    @annotation.tailrec
    def iterate(nodesToExpand: List[(CfgNode, List[CfgNode], MultiSet[CfgNode], List[CfgNode])], acc: List[List[CfgNode]], stepsLeft: Int): List[List[CfgNode]] = {
      nodesToExpand match {
        case Nil => acc
        case (currentNode, currentPath, visitedNodes, callInStack) :: rest =>
          // Extend the current path by an incoming node.
          // Filter the current node if it is not one of the predecessors
          // to the sink with respect to data dependency.
          // Also, filter non-call nodes if onlyCallNodes is true
          val shouldExtendPath =
            (!filterByDdg || ddgPrevNodes.contains(currentNode)) &&
              (!onlyCallNodes || currentNode.isCall)
          val extendedPath =
            if (shouldExtendPath) currentPath :+ currentNode
            else currentPath

          // Check if currentNode is the same as upUntilNode or upUntilNode is None
          val reachedUpUntilNode = upUntilNode.exists(_ == currentNode)

          if (stepsLeft == 0 || currentNode.isEmpty) {
            iterate(rest, acc, stepsLeft)  // no more nodes to expand, return acc
          } else if (reachedUpUntilNode) {
            iterate(rest, extendedPath :: acc, stepsLeft)  // if reached target node, add the extended path to acc
          } else {
            // Handle loops
            val updatedVisitedNodes = visitedNodes.add(currentNode)
            val isLoopNode = updatedVisitedNodes.count(currentNode) >= 3

            // Branch from the current node within the scope of the current method
            var newCallInStack = callInStack
            var branchingNodes: List[CfgNode] = if (isLoopNode) List()
            else {
              if (currentNode.isCall
                && currentNode.asInstanceOf[Call].callOut.nonEmpty
                && !currentNode.asInstanceOf[Call].callOut.head.isExternal  // to handle: what if there are multiple signatures?
                && currentNode.asInstanceOf[Call].callOut.head.cfgNode.exists(_.isReturn)) {
                val calleeMethod = currentNode.asInstanceOf[Call].callOut.head
                val calleeReturnNode = calleeMethod.cfgNode.filter(_.isReturn).head  // to handle: what if there are multiple return statements in the callee method?
                newCallInStack = currentNode +: newCallInStack
                List(calleeReturnNode)
              } else currentNode.cfgIn.dedup.map(_.asInstanceOf[CfgNode]).toList
            }

            // If there is no branching from the current node within the method,
            // try to extend the path inter-procedurally
            if (branchingNodes.isEmpty && !isLoopNode) {
              if (callInStack.nonEmpty) {
                branchingNodes = newCallInStack.head.cfgIn.dedup.map(_.asInstanceOf[CfgNode]).toList
                newCallInStack = newCallInStack.tail
              } else {
                branchingNodes = currentNode.method.callIn.dedup.map(_.asInstanceOf[CfgNode]).toList
              }
            }

            // Extend the list of nodes to expand with the branching nodes and the extended path
            val newNodesToExpand = branchingNodes.map(bn => (bn, extendedPath, updatedVisitedNodes, newCallInStack))
            if (newNodesToExpand.isEmpty) {
              if (!isLoopNode && upUntilNode.isEmpty) iterate(rest, extendedPath :: acc, stepsLeft) else iterate(rest, acc, stepsLeft)
            } else {
              iterate(rest ::: newNodesToExpand, acc, stepsLeft - 1)
            }
          }
      }
    }

    iterate(List((nodeToExpand, List(), MultiSet.empty[CfgNode], List())), Nil, steps)
  }

  def generateAllExecPathsInterProc(nodeToExpand: CfgNode, steps: Int = Int.MaxValue)(implicit cpg: Cpg): List[List[CfgNode]] = {
    // Obtain the set of all ddg-preceding node to the given one
    val ddgPrevNodes = traverseDdgBackwardsInterProc(nodeToExpand)

    @annotation.tailrec
    def iterate(nodesToExpand: List[(CfgNode, List[CfgNode], MultiSet[CfgNode])], acc: List[List[CfgNode]], stepsLeft: Int): List[List[CfgNode]] = {
      nodesToExpand match {
        case Nil => acc
        case (currentNode, currentPath, visitedNodes) :: rest =>

          if (stepsLeft == 0 || currentNode.isEmpty) {
            iterate(rest, acc, stepsLeft)
          } else {
            // Extend the current path by an incoming node.
            // Filter the current node if it is not one of the predecessors
            // to the sink with respect to data dependency
            val extendedPath =
            if (ddgPrevNodes.contains(currentNode)) {
              currentPath :+ currentNode
            } else {
              currentPath
            }

            // Handle loops
            val updatedVisitedNodes = visitedNodes.add(currentNode)
            val isLoopNode = updatedVisitedNodes.count(currentNode) >= 3

            // Branch from the current node
            var branchingNodes = if (isLoopNode) List() else currentNode.cfgIn.dedup.map(_.asInstanceOf[CfgNode]).toList
            // If there is no branching in the current node,
            // try to extend the path inter-procedurally
            if (branchingNodes.isEmpty && !isLoopNode) {
              branchingNodes = currentNode.method.callIn.dedup.map(_.asInstanceOf[CfgNode]).toList
            }
            val branchingNodesCodes = branchingNodes.iterator.code.l.mkString("\n")
            println(s"Current node: ${currentNode.code}\nExtended path: ${extendedPath.code.l}\nBranching into nodes: \n$branchingNodesCodes\n\n")

            if (branchingNodes.isEmpty) {
              if (!isLoopNode) iterate(rest, extendedPath :: acc, stepsLeft) else iterate(rest, acc, stepsLeft)
            } else {
              // Extend the list of nodes to expand with the branching nodes and the extended path
              val newNodesToExpand = branchingNodes.map(bn => (bn, extendedPath, updatedVisitedNodes))
              iterate(rest ::: newNodesToExpand, acc, stepsLeft - 1)
            }
          }
      }
    }

    iterate(List((nodeToExpand, List(), MultiSet.empty[CfgNode])), Nil, steps)
  }

  // Handle situation: excluded control statement blocks contain data that is later used in the sink
  def generateAllExecPaths(nodeToExpand: CfgNode, steps: Int = Int.MaxValue)(implicit cpg: Cpg): List[List[CfgNode]] = {
    // Obtain the set of all ddg-preceding node to the given one
    val ddgPrevNodes = traverseDdgBackwards(nodeToExpand)

    @annotation.tailrec
    def iterate(nodesToExpand: List[(CfgNode, List[CfgNode], MultiSet[CfgNode])], acc: List[List[CfgNode]], stepsLeft: Int): List[List[CfgNode]] = {
      nodesToExpand match {
        case Nil => acc
        case (currentNode, currentPath, visitedNodes) :: rest =>

          if (stepsLeft == 0 || currentNode.isEmpty) {
            iterate(rest, acc, stepsLeft)
          } else {
            // Extend the current path by an incoming node.
            // Filter the current node if it is not one of the predecessors
            // to the sink with respect to data dependency
            val extendedPath =
            if (ddgPrevNodes.contains(currentNode)) {
              currentPath :+ currentNode
            } else {
              currentPath
            }

            // Handle loops
            val updatedVisitedNodes = visitedNodes.add(currentNode)
            val isLoopNode = updatedVisitedNodes.count(currentNode) >= 3

            // Branch from the current node
            val branchingNodes = if (isLoopNode) List() else currentNode.cfgIn.dedup.map(_.asInstanceOf[CfgNode]).toList

            if (branchingNodes.isEmpty) {
              if (!isLoopNode) iterate(rest, extendedPath :: acc, stepsLeft) else iterate(rest, acc, stepsLeft)
            } else {
              // Extend the list of nodes to expand with the branching nodes and the extended path
              val newNodesToExpand = branchingNodes.map(bn => (bn, extendedPath, updatedVisitedNodes))
              iterate(rest ::: newNodesToExpand, acc, stepsLeft - 1)
            }
          }
      }
    }

    iterate(List((nodeToExpand, List(), MultiSet.empty[CfgNode])), Nil, steps)
  }

  // Version without filtering nodes by DDG
  def generateAllExecPathsNoFiltering(nodeToExpand: CfgNode, steps: Int = Int.MaxValue)(implicit cpg: Cpg): List[List[CfgNode]] = {

    @annotation.tailrec
    def iterate(nodesToExpand: List[(CfgNode, List[CfgNode], MultiSet[CfgNode])], acc: List[List[CfgNode]], stepsLeft: Int): List[List[CfgNode]] = {
      nodesToExpand match {
        case Nil => acc
        case (currentNode, currentPath, visitedNodes) :: rest =>

          if (stepsLeft == 0 || currentNode.isEmpty) {
            iterate(rest, acc, stepsLeft)
          } else {
            // Extend the current path by an incoming node
            val extendedPath = currentPath :+ currentNode

            // Handle loops
            val updatedVisitedNodes = visitedNodes.add(currentNode)
            val isLoopNode = updatedVisitedNodes.count(currentNode) >= 3

            // Branch from the current node
            val branchingNodes = if (isLoopNode) List() else currentNode.cfgIn.dedup.map(_.asInstanceOf[CfgNode]).toList

            if (branchingNodes.isEmpty) {
              if (!isLoopNode) iterate(rest, extendedPath :: acc, stepsLeft) else iterate(rest, acc, stepsLeft)
            } else {
              // Extend the list of nodes to expand with the branching nodes and the extended path
              val newNodesToExpand = branchingNodes.map(bn => (bn, extendedPath, updatedVisitedNodes))
              iterate(rest ::: newNodesToExpand, acc, stepsLeft - 1)
            }
          }
      }
    }

    iterate(List((nodeToExpand, List(), MultiSet.empty[CfgNode])), Nil, steps)
  }

  // Version that attempts to handle unrealized loop and conditional control statements
  def generateAllExecPathsUnrealControl(nodeToExpand: CfgNode, steps: Int = Int.MaxValue)(implicit cpg: Cpg): List[List[CfgNode]] = {

    @annotation.tailrec
    def iterate(nodesToExpand: List[(CfgNode, List[CfgNode], MultiSet[CfgNode])], acc: List[List[CfgNode]], stepsLeft: Int): List[List[CfgNode]] = {
      nodesToExpand match {
        case Nil => acc
        case (currentNode, currentPath, visitedNodes) :: rest =>

          if (stepsLeft == 0 || currentNode.isEmpty) {
            iterate(rest, acc, stepsLeft)
          } else {
            // Check if the current node is a loop condition or loop initialization and if its body is not in the path.
            // We filter such nodes
            val isUntriggeredLoopCondOrInit =
              (currentPath.nonEmpty
              && currentNode.astParent.isControlStructure  // check that the current node is a control structure
              && currentNode.astParent.propertiesMap.getOrDefault("CONTROL_STRUCTURE_TYPE", "").asInstanceOf[String] == "FOR")  // check that it is a FOR loop
              && !currentNode.astParent.astChildren.toSet.contains(currentPath.last)  // check that the last path node is not a part of the loop
              && !currentPath.last.controlledBy.exists(currentNode.astParent.astChildren.toSet.contains)  // check that the last path node is not controlled by any part of the loop

            // Extend the current path by an incoming CALL node.
            // If the current node controls something, it is a control node.
            // Additionally, if the current node does not control the last statement in the current path,
            // then it is an untriggered control statement, which we decided to filter
            val extendedPath =
            if (!(currentPath.nonEmpty && currentNode.controls.nonEmpty && !currentNode.controls.contains(currentPath.last))
                && currentNode.isCall
                && !isUntriggeredLoopCondOrInit) {
              currentPath :+ currentNode
            } else {
              currentPath
            }

            // Handle loops
            val updatedVisitedNodes = visitedNodes.add(currentNode)
            val isLoopNode = updatedVisitedNodes.count(currentNode) >= 3

            // Branch from the current node
            val branchingNodes = if (isLoopNode) List() else currentNode.cfgIn.dedup.map(_.asInstanceOf[CfgNode]).toList
            val branchingNodesCodes = if (isLoopNode) List() else currentNode.cfgIn.dedup.map(_.asInstanceOf[CfgNode]).code.l.mkString("\n")
            println(s"Current node: ${currentNode.code}\nExtended path: ${extendedPath.code.l}\nBranching into nodes: \n$branchingNodesCodes\n\n")

            if (branchingNodes.isEmpty) {
              if (!isLoopNode) iterate(rest, extendedPath :: acc, stepsLeft) else iterate(rest, acc, stepsLeft)
            } else {
              // Extend the list of nodes to expand with the branching nodes and the extended path
              val newNodesToExpand = branchingNodes.map(bn => (bn, extendedPath, updatedVisitedNodes))
              iterate(rest ::: newNodesToExpand, acc, stepsLeft - 1)
            }
          }
      }
    }

    iterate(List((nodeToExpand, List(), MultiSet.empty[CfgNode])), Nil, steps)
  }

  // Version that attempts to handle unrealized conditional control statements
  def generateAllExecPathsUnrealCond(nodeToExpand: CfgNode, steps: Int = Int.MaxValue)(implicit cpg: Cpg): List[List[CfgNode]] = {

    @annotation.tailrec
    def iterate(nodesToExpand: List[(CfgNode, List[CfgNode], MultiSet[CfgNode])], acc: List[List[CfgNode]], stepsLeft: Int): List[List[CfgNode]] = {
      nodesToExpand match {
        case Nil => acc
        case (currentNode, currentPath, visitedNodes) :: rest =>

          if (stepsLeft == 0 || currentNode.isEmpty) {
            iterate(rest, acc, stepsLeft)
          } else {
            // Extend the current path by an incoming CALL node.
            // If the current node controls something, it is a control node.
            // Additionally, if the current node does not control the last statement in the current path,
            // then it is an untriggered control statement, which we decided to filter
            val extendedPath =
            if (!(currentPath.nonEmpty && currentNode.controls.nonEmpty && !currentNode.controls.contains(currentPath.last))
              && currentNode.isCall) {
              currentPath :+ currentNode
            } else {
              currentPath
            }

            // Handle loops
            val updatedVisitedNodes = visitedNodes.add(currentNode)
            val isLoopNode = updatedVisitedNodes.count(currentNode) >= 3

            // Branch from the current node
            val branchingNodes = if (isLoopNode) List() else currentNode.cfgIn.dedup.map(_.asInstanceOf[CfgNode]).toList
            val branchingNodesCodes = if (isLoopNode) List() else currentNode.cfgIn.dedup.map(_.asInstanceOf[CfgNode]).code.l.mkString("\n")
            println(s"Current node: ${currentNode.code}\nExtended path: ${extendedPath.code.l}\nBranching into nodes: \n$branchingNodesCodes\n\n")

            if (branchingNodes.isEmpty) {
              if (!isLoopNode) iterate(rest, extendedPath :: acc, stepsLeft) else iterate(rest, acc, stepsLeft)
            } else {
              // Extend the list of nodes to expand with the branching nodes and the extended path
              val newNodesToExpand = branchingNodes.map(bn => (bn, extendedPath, updatedVisitedNodes))
              iterate(rest ::: newNodesToExpand, acc, stepsLeft - 1)
            }
          }
      }
    }

    iterate(List((nodeToExpand, List(), MultiSet.empty[CfgNode])), Nil, steps)
  }

  def getNodesByMethod(fileName: String, methodName: String)(implicit cpg: Cpg): Iterator[CfgNode] = {
    cpg.method(methodName).filename(fileName).cfgNode
  }

  def getNodesByMethodLineNum(fileName: String, methodName: String, lineNumber: Int)(implicit cpg: Cpg): Iterator[CfgNode] = {
    cpg.method(methodName).filename(fileName).cfgNode.lineNumber(lineNumber).filter(_.isCall)
  }

  def getNodesByMethodVarLineNum(fileName: String, methodName: String, varName: String, lineNumber: Int)(implicit cpg: Cpg): Iterator[CfgNode] = {
    cpg.method(methodName).filename(fileName).cfgNode.code(varName).lineNumber(lineNumber)
  }

  def printAllExecPathsByLines(source: String, methodName: String, varName: String, lineNumber: Int)(implicit cpg: Cpg): Unit = {
    val startNodes = getNodesByMethodVarLineNum("Test0.java", methodName, varName, lineNumber)(cpg)
    val allExecPaths = generateAllExecPathsInterProcWithCrawlIn(startNodes.head, filterByDdg = true, onlyCallNodes = false)(cpg)
    for ((execPath, i) <- allExecPaths.zipWithIndex) {
      val execPathStr = execPath.reverseIterator.code.l.mkString("\n")
      val execPathCodeStr = MyExecPath2CodeExtractor(source).extract(execPath.reverseIterator.l, true)(cpg).mkString("\n")
      println(s"Execution path #${i + 1}:\n$execPathStr\n\n")
      println(s"Line-wise execution path #${i + 1}:\n$execPathCodeStr\n\n")
    }
  }

  def printExecSlice(source: String, methodName: String, varName: String, lineNumber: Int)(implicit cpg: Cpg): Unit = {
    val startNodes = getNodesByMethodVarLineNum("Test0.java", methodName, varName, lineNumber)(cpg)
    val execSlice = traverseDdgBackwardsInterProcWithCrawlIn(startNodes.head)(cpg).toSet.toList
//    val execPathStr = execSlice.reverseIterator.code.l.mkString("\n")
    val execSliceCodeStr = MyExecPath2CodeExtractor(source).extract(execSlice, false)(cpg).mkString("\n")
//    println(s"Execution slice:\n$execPathStr\n\n")
    println(s"Line-wise execution slice:\n$execSliceCodeStr\n\n")
  }

}