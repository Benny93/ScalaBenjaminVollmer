/**
 * Created by Benny on 01.03.15.
 */

import ShuntingYard._
import org.scalatest._

import scala.collection.immutable.Queue


class ShuntingYardSpec extends FlatSpec {

    "List Testing" should "give an example of Listtypes" in{
      val stack = List(1,2,3)
      val stack2 = List(4,5,6)
      val res = stack2 ::: stack
      //println("concatination of two stacks done right: " + res)
    }

    "zeroOrMore" should "return if there are zeroOrMore ws" in {
      val wsP = whiteSpaceParser(" ")

      assert(
        zeroOrMore(wsP)("   hallo") == Some((List(Comment(" "), Comment(" "), Comment(" ")),"hallo")),
        zeroOrMore(wsP)("d   hallo") == None
      )
    }

    "tokenizeExpression" should "tokenize an Expression" in {
      assert(tokenizeExpression("456") == List(Number("456")))


      assert(
      tokenizeExpression("9+24/(7-3)")== List(
      Number("9"),
      Operator("+",2,2),
      Number("24"),
      Operator("/",2,4),
      Bracket("(", true),
      Number("7"),
      Operator("-",2,3),
      Number("3"),
      Bracket(")",false)
      )
      )
      //println(tokenizeExpression("9+24/(7-3)"))
    }


/*

    "parseAEWithShuntingYard" should "parse and eval correctly" in {
      assert(
      parseAEWithShuntingYard("9+24/(7-3)") == 15
      )
      assert(
      parseAEWithShuntingYard("3+3*3") == 12
      )
      assert(
        parseAEWithShuntingYard("(3+3)*3") == 18
      )
      assert(
        parseAEWithShuntingYard("45") == 45
      )
      assert(
        parseAEWithShuntingYard("60/5/4") == 3
      )
      assert(
        parseAEWithShuntingYard("60/5/4/3") == 1
      )
      assert(
        parseAEWithShuntingYard("2*2*2") == 8
      )
      assert(
        parseAEWithShuntingYard("3-2+4-2") == 3
      )
      assert(
        parseAEWithShuntingYard(" 3 -   6 / 3  /  2 ") == 2
      )
    }
*/
  "parseAEWithShuntingYard" should "be able to parse if-then-else"in{
    /*
    assert(
      parseAEWithShuntingYard("if 1 == 2 then 3 else 4") == 4
    )
    //and a stacked one
    assert(
      parseAEWithShuntingYard("if 1 == 2 then if 5 == 5 then 2 else 10 else 4") == 4
    )
    */
    assert(
      parseAEWithShuntingYard("if 1 == 2 then 3 else if 4 == 4 then if 5 == 5 then 6 else 7 else 8") == 6
    )
  }

}
