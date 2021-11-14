@main def hello: Unit =

  import scala.collection.immutable.Set
  import scala.collection.immutable.Map

  sealed trait Expr
  case class Num(n: Int) extends Expr
  case class Plus(left: Expr, right: Expr) extends Expr
  case class Mult(left: Expr, right: Expr) extends Expr
  case class Let(name: String, expr: Expr, body: Expr) extends Expr
  case class Var(name: String) extends Expr

  val testsForFreevars = List[(Expr,Set[String])](
    // a comma-separated list of (input, expected output)
    // where input is an expression and expected output should be the
    // result of freevarsK(input), for both K=1 and K=2
    (Num(0), Set()),
    (Num(2020), Set()),
    // now add more tests below
    // don't forget to add a comma to the end of the previous test
    (Plus(Var("x"),Plus(Let("x",Plus(Var("x"),Var("x"))//this ln cont. to next ln
    ,Plus(Var("x"),Var("x"))),Var("x"))) , Set("x")),
    (Plus(Var("x"),Mult(Let("x",Mult(Num(5),Num(3)),Plus(Num(5),Var("y"))),Var("y"))),Set("x","y") ),
    (Plus(Var("x"),Mult(Plus(Num(5),Var("y")),Var("y"))),Set("x","y")),
    (Let("x",Let("y",Num(5),Num(6)),Mult(Var("x"),Var("x"))),Set("x","y"))
  )

  def freevars1(expr: Expr): Set[String] = {
    def setAdder(expr:Expr,varAcc:Set[String]):Set[String]= expr match{
      case Plus(e1,e2) => {
        (setAdder(e1,varAcc)++(varAcc++setAdder(e2,varAcc)))
      } // case Plus

      case Mult(e3,e4) => {
        (setAdder(e3,varAcc)++(varAcc++setAdder(e4,varAcc)))
      } // case Mult

      case Let(n,e5,b) => {
        (setAdder(b,varAcc)++varAcc++setAdder(e5,varAcc))+n
      } // case Let

      case Var(n2) => {
        varAcc + n2
      } // case Var

      case Num(number) => {
        Set()
      } // case Num

    } // setAdder

    setAdder(expr,Set())
  }
  test("freevars1", freevars1 _, testsForFreevars)

  def freevars2(expr: Expr): Set[String] = expr match {
    case Plus(e,e1) =>
      freevars2(e)++freevars2(e1)
    case Mult(e,e1) =>
      freevars2(e)++freevars2(e1)
    case Let(n,e,b) =>
      (freevars2(e)++freevars2(b))+n
    case Var(n) => Set()+n
    case Num(int) => Set()
  }
  test("freevars2", freevars2 _, testsForFreevars)

  val testsForExpand = List[(Expr,Expr)](
    // a comma-separated list of (input, expected output)
    // where input is an expression and expected output should be the
    // result of expand(input)
    (Num(0), Num(0)),//test 1
    (Num(2020), Num(2020)),// test 2
    // now add more tests below
    // don't forget to add a comma to the end of the previous test
    (Let("x",Num(5+3),Plus(Var("x"),Var("x"))) , Plus(Num(5+3),Num(5+3)) ),// test 3
    (Plus(Var("x"),Mult(Let("x",Mult(Num(5),Num(3)),// this ln Beginning of nxt ln
      Plus(Num(5),Var("y"))),Var("y"))),//test 4 x+(Letx=5*3in5+y)*y
      Plus(Var("x"),Mult(Plus(Num(5),Var("y")),Var("y")))), //test 4 x+(5+y)*y
    (Let("x",Let("y",Num(5),Num(6)),Mult(Var("x"),Var("x"))),Mult(Num(6),Num(6)))
  )

  def expand(expr: Expr): Expr = {
    def expander(expr: Expr,varHldr: Map[String,Expr]): Expr = expr match {
      case Plus(e,e1) => Plus(expander(e,varHldr),expander(e1,varHldr))
      case Mult(e,e1) => Mult(expander(e,varHldr),expander(e1,varHldr))
      case Num(int) => Num(int)
      case Let(n,e,b) => expander(b,varHldr+(n->expander(e,varHldr)))
      case Var(n) => {
        if (varHldr.contains(n)) varHldr(n)
        else Var(n)
      }
    }

    expander(expr,Map.empty[String,Expr])
  }
  test("expand", expand _, testsForExpand)

  // Don't change the code below
  // (if you feel you really need to, clear it with me first)

  def test[A](name: String, f: Expr => A, tests: List[(Expr, A)]): Unit = {
    println(s">>>> Begin testing $name")
    for ((expr, expectedResult) <- tests) {
      try {
        val result = f(expr)
        if (result != expectedResult)
          fail(s"\n  Expected: $expectedResult\n  But got:  $result")
        print(".")
      }
      catch {
        case e: Throwable =>
          println()
          println(s"Failed test on input $expr")
          println(e)
      }
    }
    println()
    println(s">>>> End testing $name.")
  }

  def ignoretest[A](name: String, f: Expr => A, tests: List[(Expr, A)]): Unit = {
    println(s"**** Ignoring tests for $name")
  }

}
