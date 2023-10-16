package execpathextractor

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.Cpg
//import io.shiftleft.codepropertygraph.cpgloading.CpgLoader
import io.shiftleft.semanticcpg.language.*
//import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
//import io.joern.x2cpg.passes.controlflow.CfgCreationPass

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

  // Handle situation: unrealized (excluded) LOOP or IF statement contain data that is later used in the sink (or should this already have been handled by the developer?)
  def generateAllExecPaths(nodeToExpand: CfgNode, steps: Int = Int.MaxValue)(implicit cpg: Cpg): List[List[CfgNode]] = {

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
              (currentNode.astParent.isControlStructure  // check that the current node is a control structure
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

  // Add feature: filter unrealized loop statements?
  // Handle situation: unrealized loop or if statement contains data that is used in the sink later on (should it be already handled by the developer?)
  def generateAllExecPaths0(nodeToExpand: CfgNode, steps: Int = Int.MaxValue)(implicit cpg: Cpg): List[List[CfgNode]] = {

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
//    val startNodes = getNodesByMethodVarLineNum("Test0.java", methodName, varName, lineNumber)(cpg)
    val startNodes = getNodesByMethodLineNum("Test0.java", methodName, lineNumber)(cpg)
    val allExecPaths = generateAllExecPaths(startNodes.next)(cpg)
    for ((execPath, i) <- allExecPaths.zipWithIndex) {
      val execPathStr = execPath.reverseIterator.code.l.mkString("\n")
      val execPathCodeStr = MyExecPath2CodeExtractor(source).extract(execPath)(cpg).mkString("\n")
      println(s"Execution path #${i + 1}:\n$execPathStr\n\n")
      println(s"Line-wise execution path #${i + 1}:\n$execPathCodeStr\n\n")
    }
  }

}