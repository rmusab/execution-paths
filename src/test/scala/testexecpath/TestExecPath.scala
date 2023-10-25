package testexecpath

import io.shiftleft.codepropertygraph.Cpg
import execpathbuilder.CFGraphPathBuilder.printAllExecPathsByLines
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
    val source = scala.io.Source.fromFile(fileAbsPath).mkString
    println(s"Source: \n$source\n")
    val cpg = getCpg(source)
    printAllExecPathsByLines(source, methodName, varName, lineNumber)(cpg)
  }

//  testGetAllExecPaths("/home/ravil/IdeaProjects/execution-paths/data/NestedIfTest.java", "main", "e", 19)
//  testGetAllExecPaths("/home/ravil/IdeaProjects/execution-paths/data/Loop1.java", "main", "i", 4)
//  testGetAllExecPaths("/home/ravil/IdeaProjects/execution-paths/data/Loop2.java", "main", "i", 5)
//  testGetAllExecPaths("/home/ravil/IdeaProjects/execution-paths/data/Loop3.java", "main", "i", 6)
//  testGetAllExecPaths("/home/ravil/IdeaProjects/execution-paths/data/NestedLoops1.java", "main", "a", 12)
//  testGetAllExecPaths("/home/ravil/IdeaProjects/execution-paths/data/NestedLoopIfs.java", "getISmtokenPos", "num", 16)
//  testGetAllExecPaths("/home/ravil/IdeaProjects/execution-paths/data/NestedLoopIfs2.java", "getISmtokenPos", "file", 27)
  testGetAllExecPaths("/home/ravil/IdeaProjects/execution-paths/data/InterProcedural1.java", "accessFile", "file", 55)
//  testGetAllExecPaths("/home/ravil/IdeaProjects/execution-paths/data/InterProcedural2.java", "processInput", "input", 13)

}