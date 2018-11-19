package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }


  test("calculator compute Plus signals") {
    val a: Signal[Expr] = Signal(Literal(1.3d))
    val b: Signal[Expr] = Signal(Literal(1.3d))
    val c: Signal[Expr] = Signal(Plus(Ref("a"),Ref("b")))
    val values = Calculator.computeValues(
      Map(
        "a" -> a,
        "b" -> b,
        "c" -> c
      )
    )

    /*values.foreach{
      case (x, y) => println(s"name:[$x]--> value:[${y()}]")
    }*/

    values.map{
      case(x, y) => (x,y())
    } should contain theSameElementsAs (List(("a",1.3d),("b",1.3d),("c",2.6d)))

    a() = Literal(1.7d)

    /*values.foreach{
      case (x, y) => println(s"name:[$x]--> value:[${y()}]")
    }*/

    values.map{
      case(x, y) => (x,y())
    } should contain theSameElementsAs (List(("a",1.7d),("b",1.3d),("c",3.0d)))

  }

  test("calculator compute Minus signals") {
    val a: Signal[Expr] = Signal(Literal(1.3d))
    val b: Signal[Expr] = Signal(Literal(1.3d))
    val c: Signal[Expr] = Signal(Minus(Ref("a"),Ref("b")))
    val values = Calculator.computeValues(
      Map(
        "a" -> a,
        "b" -> b,
        "c" -> c
      )
    )

    values.map{
      case(x, y) => (x,y())
    } should contain theSameElementsAs (List(("a",1.3d),("b",1.3d),("c",0)))

    a() = Literal(1.7d)

    values.map{
      case(x, y) => (x,y())
    } should contain theSameElementsAs (List(("a",1.7d),("b",1.3d),("c",0.3999999999999999d)))

  }

  test("calculator compute non existent signals as NaN") {
    val a: Signal[Expr] = Signal(Literal(1.3d))
    val b: Signal[Expr] = Signal(Literal(1.3d))
    val c: Signal[Expr] = Signal((Ref("d")))
    val values = Calculator.computeValues(
      Map(
        "a" -> a,
        "b" -> b,
        "c" -> c
      )
    )
    assert(values("c")().isNaN)

  }

  test("calculator compute cyclic references signals as NaN") {
    val a: Signal[Expr] = Signal(Minus(Ref("a"),Literal(1d)))
    val values = Calculator.computeValues(
      Map(
        "a" -> a
      )
    )
    assert(values("a")().isNaN)

  }

  test("calculator compute several cyclic references signals as NaN") {
    val a: Signal[Expr] = Signal(Minus(Ref("b"),Literal(1d)))
    val b: Signal[Expr] = Signal(Minus(Ref("a"),Literal(1d)))
    val values = Calculator.computeValues(
      Map(
        "a" -> a,
        "b" -> b
      )
    )
    assert(values("a")().isNaN)
    assert(values("b")().isNaN)

  }

}
