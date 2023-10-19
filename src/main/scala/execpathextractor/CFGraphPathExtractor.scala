package execpathextractor

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.joern.dataflowengineoss.language.{ExtendedCfgNode, toExtendedCfgNode}
import io.shiftleft.codepropertygraph.Cpg
//import io.shiftleft.codepropertygraph.cpgloading.CpgLoader
import io.shiftleft.semanticcpg.language.*
//import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
//import io.joern.x2cpg.passes.controlflow.CfgCreationPass

import io.joern.dataflowengineoss.queryengine.{Engine, TableEntry, EngineContext}

import utils.ExecPath2CodeExtractor

object CFGraphPathExtractor {

  // Immutable MultiSet implementation.
  // Time complexity:
  // `add` is O(log(n))
  // `count` is O(1) on average
  // `remove` is O(log(n))
  object MultiSet {
    def empty[T]: MultiSet[T] = new MultiSet(Map.empty[T, Int])
  }
  class MultiSet[T](private val counts: Map[T, Int]) {

    def add(element: T): MultiSet[T] = {
      val newCount = counts.getOrElse(element, 0) + 1
      new MultiSet(counts + (element -> newCount))
    }

    def count(element: T): Int = counts.getOrElse(element, 0)

    def remove(element: T): MultiSet[T] = {
      counts.get(element) match {
        case Some(c) if c > 1 => new MultiSet(counts + (element -> (c - 1)))
        case Some(_) => new MultiSet(counts - element)
        case None => this
      }
    }
  }

  class MyExecPath2CodeExtractor(source: String) extends ExecPath2CodeExtractor {
    override protected def readFile(filename: String): String = {
      source
    }
  }

  def getBackwardPaths(sink: CfgNode, source: CfgNode): List[TableEntry] = {
    val context = EngineContext()
    val engine = new Engine(context)
    val result = engine.backwards(List(sink), List(source))
    result
  }

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
            //            val branchingNodesCodes = if (isLoopNode) List() else currentNode.cfgIn.dedup.map(_.asInstanceOf[CfgNode]).code.l.mkString("\n")
            //            println(s"Current node: ${currentNode.code}\nExtended path: ${extendedPath.code.l}\nBranching into nodes: \n$branchingNodesCodes\n\n")

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
//            val branchingNodesCodes = if (isLoopNode) List() else currentNode.cfgIn.dedup.map(_.asInstanceOf[CfgNode]).code.l.mkString("\n")
//            println(s"Current node: ${currentNode.code}\nExtended path: ${extendedPath.code.l}\nBranching into nodes: \n$branchingNodesCodes\n\n")

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
  def generateAllExecUnrealControl(nodeToExpand: CfgNode, steps: Int = Int.MaxValue)(implicit cpg: Cpg): List[List[CfgNode]] = {

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
//    val startNodes = getNodesByMethodLineNum("Test0.java", methodName, lineNumber)(cpg)
    val allExecPaths = generateAllExecPaths(startNodes.next)(cpg)
    for ((execPath, i) <- allExecPaths.zipWithIndex) {
      val execPathStr = execPath.reverseIterator.code.l.mkString("\n")
      val execPathCodeStr = MyExecPath2CodeExtractor(source).extract(execPath)(cpg).mkString("\n")
      println(s"Execution path #${i + 1}:\n$execPathStr\n\n")
      println(s"Line-wise execution path #${i + 1}:\n$execPathCodeStr\n\n")
    }
  }

}