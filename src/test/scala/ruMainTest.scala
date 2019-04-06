import org.bitbucket.inkytonik.kiama.util.FileSource
import org.scalatest.FunSuite
import syntax.ExpParserPrettyPrinter.{any, layout}
import syntax.ExpParserSyntax.{nIf, _}
class ruMainTest extends FunSuite {

  test("runMain.simpleAddTest") {
    val fileName = "simpleAddTest.exp"
    val args = Seq(fileName)

    val c = ruMain.createConfig(args);
    val ast = ruMain.makeast(FileSource(fileName, "UTF-8"), c)

    val correctAst = Left(stmts(Assign(Cell("C","2"),Num("1")),
      stmts(Assign(Cell("C","1"),Num("1")),
        Assign(Cell("A","1"),Add(Ref(Cell("C","1")),Ref(Cell("C","2")))))))

    assert(ast == correctAst)
  }

  test("runMain.arrayTest") {
    val fileName = "arrayTest.exp"
    val args = Seq(fileName)

    val c = ruMain.createConfig(args);
    val ast = ruMain.makeast(FileSource(fileName, "UTF-8"), c)

    val correctAst = Left(stmts(
      Assign(Cell("A", "3"), Num("3")),
      stmts(
        Assign(Cell("A", "8"), Num("8")),
        stmts(
          Assign(Cell("A", "4"), Num("4")),
          stmts(
            Assign(Cell("A", "1"), Num("1")),
            stmts(
              Assign(Cell("A", "7"), Num("7")),
              stmts(
                Assign(Cell("A", "5"), Num("5")),
                stmts(
                  Assign(Cell("A", "6"), Num("6")),
                  stmts(
                    Assign(Cell("A", "2"), Num("2")),
                    Assign(
                      Cell("B", "1"),
                        SUM(
                          Vector(
                            Args(
                              Ref(Cell("A", "1")),
                              Args(
                                Ref(Cell("A", "2")),
                                Args(
                                  Ref(Cell("A", "3")),
                                  Args(Ref(Cell("A", "4")),
                                    Args(Ref(Cell("A", "5")),
                                      Args(Ref(Cell("A", "6")),
                                        Args(Ref(Cell("A", "7")),
                                          Arg(Ref(Cell("A", "8"))
                                          ))))))))))))))))))))

    assert(ast == correctAst)
  }

  test("runMain.embeddedArgTest") {
    val fileName = "embededArgTest.exp"
    val args = Seq(fileName)

    val c = ruMain.createConfig(args);
    val ast = ruMain.makeast(FileSource(fileName, "UTF-8"), c)

    val correctAst = Left(stmts(Assign(Cell("A","3"),Num("3")),
      stmts(Assign(Cell("A","8"),Num("8")),
        stmts(Assign(Cell("A","4"),Num("4")),
          stmts(Assign(Cell("A","1"),Num("1")),
            stmts(Assign(Cell("A","7"),Num("7")),
              stmts(Assign(Cell("A","5"),Num("5")),
                stmts(Assign(Cell("A","9"),Num("9")),
                  stmts(Assign(Cell("A","6"),Num("6")),
                    stmts(Assign(Cell("A","2"),Num("2")),
                      Assign(Cell("B","1"),
                        SUM(Vector(Args(Ref(Cell("A","1")),
                          Args(numIF(
                            nIf(Vector(Ref(Cell("A","2"))), Vector(Ref(Cell("A","3"))), Vector(Ref(Cell("A","4"))))),
                            Arg(SUM(Vector(
                              Args(Ref(Cell("A","5")),
                                Args(Ref(Cell("A","6")),
                                  Arg(numIF(
                                    nIf(Vector(Ref(Cell("A","7"))),Vector(Ref(Cell("A","8"))),Vector(Ref(Cell("A","9"))))
                                  ))))))))))))))))))))))

    assert(ast == correctAst)
  }

  test("runMain.ifTest") {
    val fileName = "ifTest.exp"
    val args = Seq(fileName)

    val c = ruMain.createConfig(args);
    val ast = ruMain.makeast(FileSource(fileName, "UTF-8"), c)

    val correctAst = Left(stmts(Assign(Cell("A","1"),Num("5")),
      stmts(Assign(Cell("B","2"),Num("45")),
        stmts(Assign(Cell("B","1"),Num("6")),
          Assign(Cell("C","1"),numIF(
            nIf(Vector(Ref(Cell("A","1"))),Vector(Ref(Cell("B","1"))),Vector(Ref(Cell("B","2"))))))))))

    assert(ast == correctAst)
  }

  test("runMain.optimise") {
    val fileName = "ifTest.exp"
    val args = Seq(fileName)

    val c = ruMain.createConfig(args);
    val ast = ruMain.makeast(FileSource(fileName, "UTF-8"), c)

    val e = ast match {
      case Left(p) => p
    }

    val opt = new Optimiser()
    val o = opt.optimise(e)

    val expectedO = stmts(Assign(Cell("A","1"),Num("5")),
      stmts(Assign(Cell("B","2"),Num("45")),
        stmts(Assign(Cell("B","1"),Num("6")),
          ifStmt(ifAssign(Vector(ifRef(Vector("if1"))),
            nIf(Vector(Ref(Cell("A","1"))),Vector(Ref(Cell("B","1"))),Vector(Ref(Cell("B","2"))))),
            Assign(Cell("C","1"),numIfRef(ifRef(Vector("if1"))))))))

    assert(o === expectedO)
  }

  test("runMain.embeddedIfTest") {
    val fileName = "embeddedIfTest.exp"
    val args = Seq(fileName)

    val c = ruMain.createConfig(args);
    val ast = ruMain.makeast(FileSource(fileName, "UTF-8"), c)

    val e = ast match {
      case Left(p) => p
    }

    val opt = new Optimiser()
    val o = opt.optimise(e)
    val expectedO = stmts(Assign(Cell("B", "2"),Boo("false")),
      stmts(Assign(Cell("E", "1"),Num("4")),
        stmts(Assign(Cell("C", "1"), Num("2")),
          stmts(Assign(Cell("D","1"), Num("2")),
            stmts(Assign(Cell("B","1"), Boo("true")),
              ifStmt(ifAssign(Vector(ifRef(Vector("if1"))),
                nIf(Vector(Ref(Cell("C","1"))),Vector(Ref(Cell("D","1"))),Vector(Ref(Cell("E","1"))))),
                ifStmt(ifAssign(Vector(ifRef(Vector("if2"))),
                  nIf(Vector(Ref(Cell("B","2"))),Vector(Ref(Cell("C","1"))),Vector(Ref(Cell("D","1"))))),
                  ifStmt(ifAssign(Vector(ifRef(Vector("if3"))),
                    nIf(Vector(Ref(Cell("B","1"))),Vector(numIfRef(ifRef(Vector("if1")))),Vector(numIfRef(ifRef(Vector("if2")))))),
                    Assign(Cell("A","1"),numIfRef(ifRef(Vector("if3"))))))))))))

    assert(o === expectedO)
  }

  test("runMain.complexIfArgs") {
    val fileName = "complexIfArgs.exp"
    val args = Seq(fileName)

    val c = ruMain.createConfig(args);
    val ast = ruMain.makeast(FileSource(fileName, "UTF-8"), c)

    val e = ast match {
      case Left(p) => p
    }

    val opt = new Optimiser()
    val o = opt.optimise(e)
    val expectedO = stmts(Assign(Cell("E","2"),Num("6")),
      stmts(Assign(Cell("E","1"),Num("3")),
        stmts(Assign(Cell("D","2"),Num("5")),
          stmts(Assign(Cell("C","1"),Num("1")),
            stmts(Assign(Cell("D","1"),Num("2")),
              stmts(Assign(Cell("C","2"),Num("4")),
                ifStmt(ifAssign(Vector(ifRef(Vector("if1"))),
                  nIf(Vector(Ref(Cell("C","1"))),Vector(Ref(Cell("D","1"))),Vector(Ref(Cell("E","1"))))),
                  ifStmt(ifAssign(Vector(ifRef(Vector("if2"))),
                    nIf(Vector(Ref(Cell("C","2"))),Vector(Ref(Cell("D","2"))),Vector(Ref(Cell("E","2"))))),
                    ifStmt(ifAssign(Vector(ifRef(Vector("if3"))),
                      nIf(Vector(Ref(Cell("D","1"))),Vector(Ref(Cell("E","1"))),Vector(numIfRef(ifRef(Vector("if2")))))),
                      Assign(Cell("A","1"),SUM(Vector(Args(numIfRef(
                        ifRef(Vector("if1"))),Arg(numIfRef(ifRef(Vector("if3")))))))))))))))))

    assert(o === expectedO)
  }

  test("runMain.multipleIfs") {
    val fileName = "multipleIfs.exp"
    val args = Seq(fileName)

    val c = ruMain.createConfig(args);
    val ast = ruMain.makeast(FileSource(fileName, "UTF-8"), c)

    val e = ast match {
      case Left(p) => p
    }

    val opt = new Optimiser()
    val o = opt.optimise(e)
    println(o)
    val expectedO = stmts(Assign(Cell("C","2"),Num("2")),
      stmts(Assign(Cell("B","2"),Boo("true")),
        stmts(Assign(Cell("C","1"),Num("1")),
          stmts(Assign(Cell("C","4"),Num("4")),
            stmts(Assign(Cell("D","3"),Num("7")),
              stmts(Assign(Cell("C","3"),Num("3")),
                stmts(Assign(Cell("D","2"),Num("6")),
                  stmts(Assign(Cell("D","1"),Num("5")),
                    stmts(Assign(Cell("B","3"),Boo("false")),
                      stmts(Assign(Cell("B","4"),Boo("false")),
                        stmts(Assign(Cell("D","4"),Num("8")),
                          stmts(Assign(Cell("B","1"),Boo("true")),
                            stmts(ifStmt(ifAssign(Vector(ifRef(Vector("if7"))),
                              nIf(Vector(Ref(Cell("B","2"))),Vector(Ref(Cell("C","2"))),Vector(Ref(Cell("D","2"))))),
                              Assign(Cell("A","2"),numIfRef(ifRef(Vector("if7"))))),
                              stmts(ifStmt(ifAssign(Vector(ifRef(Vector("if6"))),
                                nIf(Vector(Ref(Cell("B","1"))),Vector(Ref(Cell("C","1"))),Vector(Ref(Cell("D","1"))))),
                                Assign(Cell("A","1"),numIfRef(ifRef(Vector("if6"))))),
                                stmts(ifStmt(ifAssign(Vector(ifRef(Vector("if5"))),
                                  nIf(Vector(Ref(Cell("B","3"))),Vector(Ref(Cell("A","2"))),Vector(Ref(Cell("A","1"))))),
                                  Assign(Cell("A","3"),numIfRef(ifRef(Vector("if5"))))),
                                  stmts(ifStmt(ifAssign(Vector(ifRef(Vector("if1"))),
                                    nIf(Vector(Ref(Cell("A","3"))),Vector(Ref(Cell("A","2"))),Vector(Ref(Cell("A","1"))))),
                                    ifStmt(ifAssign(Vector(ifRef(Vector("if2"))),
                                      nIf(Vector(Ref(Cell("B","4"))),Vector(numIfRef(ifRef(Vector("if1")))),Vector(Ref(Cell("D","4"))))),
                                      Assign(Cell("A","4"),numIfRef(ifRef(Vector("if2")))))),
                                    ifStmt(ifAssign(Vector(ifRef(Vector("if3"))),
                                      nIf(Vector(Ref(Cell("B","4"))),Vector(Ref(Cell("C","3"))),Vector(Ref(Cell("C","4"))))),
                                      ifStmt(ifAssign(Vector(ifRef(Vector("if4"))),
                                        nIf(Vector(Ref(Cell("A","3"))),Vector(Ref(Cell("B","3"))),Vector(Ref(Cell("C","3"))))),
                                        Assign(Cell("A","5"),
                                          Add(numIfRef(ifRef(Vector("if3"))),numIfRef(ifRef(Vector("if4"))))
                                        )))))))))))))))))))

    assert(o === expectedO)
  }

}
