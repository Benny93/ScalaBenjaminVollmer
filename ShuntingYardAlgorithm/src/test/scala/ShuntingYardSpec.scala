/**
 * Created by Benny on 01.03.15.
 */

import ShuntingYard._
import org.scalatest._


class ShuntingYardSpec extends FlatSpec {

    "tokenizeExpression" should "tokenize an Expression" in {
      assert(tokenizeExpression("456") == List(Number("456")))


      assert(
      tokenizeExpression("9+24/(7-3)")== List(
      Number("9"),
      Operator("+",2,0),
      Number("24"),
      Operator("/",2,2),
      Bracket("(", true),
      Number("7"),
      Operator("-",2,1),
      Number("3"),
      Bracket(")",false)
      )
      )
      println(tokenizeExpression("9+24/(7-3)"))
    }
/*
    "parseAEWithShuntingYard" should "parse and eval correctly" in {
      assert(
      parseAEWithShuntingYard("9+24/(7-3)") == 15
      )
    }*/
}
