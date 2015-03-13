/**
 * Created by Benny on 01.03.15.
 */

import ShuntingYard._
import org.scalatest._

import scala.collection.immutable.Queue


class ShuntingYardSpec extends FlatSpec {
/*


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
*/



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

  "parseAEWithShuntingYard" should "be able to parse if-then-else"in{

    assert(
      parseAEWithShuntingYard("if 1 == 2 then 3 else 4") == 4
    )
    //and a stacked one
    assert(
      parseAEWithShuntingYard("if 1 == 2 then if 5 == 5 then 2 else 10 else 4") == 4
    )

    assert(
      parseAEWithShuntingYard("if 1 == 2 then 3 else if 4 == 4 then if 5 == 5 then 6 else 7 else 8") == 6
    )
    assert(
      parseAEWithShuntingYard("if 1 == 2*(6-3) then 3 else if 4 == 4 then if 5 == 5 then 6 else 7 else 8") == 6
    )
    assert(
      parseAEWithShuntingYard("if 1 == 2 then 3+5 else if 4 == 4 then if 5 == 5 then 2*(9-6) else 7 else 8") == 6
    )
  }

  "parseAEWithShuntingYard" should "be able to power some numbers" in {
    assert(
    parseAEWithShuntingYard("2^2") == 4
    )

    assert(
      parseAEWithShuntingYard("2*2^2") == 8
    )
    assert(
      parseAEWithShuntingYard("2*(8-6)^2") == 8
    )
  }

  "tokenize2" should "tokenize expressions" in {
    assert(
    tokenizeExpression2("1+1") == List(Number("1"),Operator("+",2,2),Number("1"))
    )
    println("here some tokenizazion")
    println("2   +2"+ " -> " + tokenizeExpression2("2   +2"))
    println("245"+ " -> " + tokenizeExpression2("245"))
    println("if 3 == 4 then 3 else 4"+ " -> " + tokenizeExpression2("if 3 == 4 then 3 else 4"))
  }

  /*Testing the AST*/

  "growAST" should "create AST from reverse polish notation" in {
    println("Tree examples: ")

    assert(
      createTreeWithSY("1 + 1") == Branch('+,List(Leaf('num,"1"), Leaf('num,"1")))
    )
    assert(
    createTreeWithSY("if 3 == 4 then 3 else 4") == Branch('if,List(Branch('==,List(Leaf('num,"4"), Leaf('num,"3"))), Leaf('num,"3"), Leaf('num,"4")))
    )
  }

  "parseAEwithSYandTrees" should "give the correct solution" in {
    assert(
      parseAEWithSYandTrees("9+24/(7-3)") == 15
    )
    assert(
      parseAEWithSYandTrees("3+3*3") == 12
    )
    assert(
      parseAEWithSYandTrees("(3+3)*3") == 18
    )
    assert(
      parseAEWithSYandTrees("45") == 45
    )
    assert(
      parseAEWithSYandTrees("60/5/4") == 3
    )
    assert(
      parseAEWithSYandTrees("60/5/4/3") == 1
    )
    assert(
      parseAEWithSYandTrees("2*2*2") == 8
    )
    assert(
      parseAEWithSYandTrees("3-2+4-2") == 3
    )
    assert(
      parseAEWithSYandTrees(" 3 -   6 / 3  /  2 ") == 2
    )
    assert(
      parseAEWithSYandTrees("if 1 == 2 then 3+5 else if 4 == 4 then if 5 == 5 then 2*(9-6) else 7 else 8") == 6

    )
  }

}
