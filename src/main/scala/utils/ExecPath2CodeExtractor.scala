package utils

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.AstNode
import io.shiftleft.semanticcpg.language._

import java.nio.file.Paths

// A trait for extracting slices
trait ExecPathExtractor[N, EncodedObj] {
  def extract(slice: List[N])(implicit cpg: Cpg): EncodedObj
}

class ExecPath2CodeExtractor extends ExecPathExtractor[AstNode, List[String]] {

  // Extracts code slices based on provided AstNodes
  def extract(execPath: List[AstNode])(implicit cpg: Cpg): List[String] = {
    val path = Paths.get(cpg.metaData.root.head, execPath.iterator.file.name.head).toString
    getLines(path, execPath.iterator)
  }

  // Returns the lines of code based on provided file name and AstNodes
  private def getLines(fileName: String, nodes: Iterator[AstNode]): List[String] = {
    val nodesList = nodes.filter(!_.isBlock).toList
    val lines = nodesList.flatMap(node => Option(node.propertiesMap.get("LINE_NUMBER").asInstanceOf[Int]))
    val nodeToLineNumberMap = nodesList.zip(lines).toMap
    val fileContent = readFile(fileName)
    val codeLines = fileContent.split("\n")
    val uniqueLineNumbers = nodeToLineNumberMap.values.toSeq.distinct.sorted

    val uniqueCodes = uniqueLineNumbers.flatMap(
      lineNumber => Option(codeLines.lift(lineNumber - 1).getOrElse(s"Cannot find code for line $lineNumber"))
    )
    uniqueCodes.toList
  }

  // Reads the file content and returns it as a string
  protected def readFile(filename: String): String = {
    val source = scala.io.Source.fromFile(filename)
    try {
      source.mkString
    } finally {
      source.close()
    }
  }
}
