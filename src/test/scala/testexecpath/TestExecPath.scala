package testexecpath

import io.shiftleft.codepropertygraph.Cpg
import execpathbuilder.CFGraphPathBuilder.{printAllExecPathsByLines, printExecSlice}
import testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class TestExecPath extends JavaSrcCode2CpgFixture(withOssDataflow = true) {

  def getCpg(source: String): Cpg = {
    //    var cpg = JavaSrc2Cpg().createCpg(fileAbsPath)(JavaSrc2Cpg.DefaultConfig).get
    //    CfgCreationPass(cpg).createAndApply()
    //    cpg
    code(source)
  }

  def testGetAllExecPaths(fileAbsPath: String, methodName: String, varName: String, lineNumber: Int): Unit = {
    val source = scala.io.Source.fromFile(fileAbsPath)
    val content = source.mkString
    source.close()
    println(s"Source: \n$source\n")
    val cpg = getCpg(content)
    printAllExecPathsByLines(content, methodName, varName, lineNumber)(cpg)
  }

  def testGetExecSlice(fileAbsPath: String, methodName: String, varName: String, lineNumber: Int): Unit = {
    val source = scala.io.Source.fromFile(fileAbsPath)
    val content = source.mkString
    source.close()
    println(s"Source: \n$source\n")
    val cpg = getCpg(content)
    printExecSlice(content, methodName, varName, lineNumber)(cpg)
  }

//  testGetAllExecPaths("src/test/resources/NestedIfTest.java", "main", "e", 19)

//  testGetAllExecPaths("src/test/resources/Loop1.java", "main", "i", 4)

//  testGetAllExecPaths("src/test/resources/Loop2.java", "main", "i", 5)

//  testGetAllExecPaths("src/test/resources/Loop3.java", "main", "i", 6)

//  testGetAllExecPaths("src/test/resources/NestedLoops1.java", "main", "a", 12)

//  testGetAllExecPaths("src/test/resources/NestedLoopIfs.java", "getISmtokenPos", "num", 16)

//  testGetAllExecPaths("src/test/resources/NestedLoopIfs2.java", "getISmtokenPos", "file", 27)

//  testGetAllExecPaths("src/test/resources/InterProcedural1.java", "accessFile", "file", 55)
  testGetExecSlice("src/test/resources/InterProcedural1.java", "accessFile", "file", 55)

//  testGetAllExecPaths("src/test/resources/InterProcedural2.java", "processInput", "input", 13)

}