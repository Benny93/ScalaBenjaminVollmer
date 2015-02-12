import SimpleGrammar._
import org.scalatest._

class SimpleGrammarSpec extends FlatSpec {
/*
  "parseAndEval" should "parse and eval arit. exoressions" in {

    assert(parseAndEval("2 - 3 + 4") == 3)
    //assert(parseAndEval("1234") == 1234)
    //assert(parseAndEval("3 * 5") == 15)
  }

  "parseAndEval" should "parse like in ex 8 described" in{
    //assert(parseAndEval("2 + 3 * 4") == 14)
    //assert(parseAndEval("2 * 3 + 4") == 10 )
    //assert(parseAndEval("1 + 2 * 3 + 4") == 11)
    //assert(parseAndEval("1 * 2 + 3 * 4") == 14)
    //assert(parseAndEval("1 * 2 * 3 * 4") == 24)
  }

  "paseAndEval" should "understand substraction and division" in {
    assert(parseAndEval("9 / 3 - 2") == 1)
    assert(parseAndEval("3 + 5 / 2") == 5)
    assert(parseAndEval("3 - 2 + 4 - 2") == 3)
    assert(parseAndEval("1 - 3 * 4") == -11)
    //assert(parseAndEval("1256 + 25 * 48 / 9") == 1381)
  }

  "it" should "give correct answer to" in {
    //assert(parseAndEval("5 - 2 - 1 - 1") == 1)
    assert(parseAndEval("1 - 2 - 3 - 4") == -8)
    assert(parseAndEval("36 / 6 / 2") == 3)
    assert(parseAndEval("3 - 6 / 3 / 2") == 2)
  }

  "it" should "be able to use a bigger language" in {
    assert(parseAndEval("1 == 1") == 1)
    assert(parseAndEval("2 + 2 == 4") == 1)
    assert(parseAndEval("25 * 8 == 500 / 2 - 50") == 1)
    assert(parseAndEval("3 == 4") == 0)
    assert(parseAndEval("if 1 == 1 then 2 else 3") == 2)
    assert(parseAndEval("if 2 + 2 == 5 then 1900 + 84 else 5 * 403") == 2015)
  }


  "it" should "parse the example Tree" in {
    assert(parseAndEval("if 2 + 2 == 5 then 1984 else 2015") == 2015)

    assert(parseAndEval("if 2 + 2 == 5 then if 1 + 2 == 3 then 1 else 0 else 0") == 0)

    //assert(parseAndEval("if 1 + 1 == 2 then if 2 + 2 == 5 then 1111 + 222 + 33 + 4 else 4444 * 333 * 22 * 1 else if 1 == 2 then 2 + 2 else 4 * 5") == 32556744 )
  }
*/
  /*Unparse Tree*/
  "unparse" should "be able to convert a tree back to code" in {
    val expectedTree = parseAE("if 2 + 2 == 5 then 1984 else 2015")
    //unparse and parse again
    println("unparsedTree: " + unparse(expectedTree))
    assert(parseAE(unparse(expectedTree)) == expectedTree)

    //own tests
    assert(unparse(parseAE("if 1 + 1 == 2 then if 2 + 2 == 5 then 1111 + 222 + 33 + 4 else 4444 * 333 * 22 * 1 else if 1 == 2 then 2 + 2 else 4 * 5")) == "if 1 + 1 == 2 then if 2 + 2 == 5 then 1111 + 222 + 33 + 4 else 4444 * 333 * 22 * 1 else if 1 == 2 then 2 + 2 else 4 * 5")



  }



  "demo" should "demonstrate the goal function" in {
    val lay1:Layout={List(
    (0,"if 1 + 1 == 2 then"),
      (2,"if 2 + 2 == 5 then"),
      (4,"1111 + 222 + 33 + 4"),
      (2,"else") ,
      (4,"4444 * 333 * 22 * 1"),
      (0,"else"),
      (2,"if 1 == 2 then 2 + 2 else 4 * 5")
      )}

    val lay2:Layout={List(
    (0,"if 1 + 1 =="),
    (5,"2 then"),
    (2,"if 2 + 2 =="),
    (7,"5 then"),
    (4,"1111 +"),
    (6,"222 +"),
    (6,"33 + 4"),
    (2,"else"),
    (4,"4444 *"),
    (6,"333 *"),
    (6,"22 * 1"),
    (0,"else"),
    (2,"if 1 =="),
    (7,"2 then"),
    (4,"2 + 2"),
    (2,"else"),
    (4,"4 * 5")
      )}
    val document = {List(lay1,lay2)}

   // println("Rendered demo Layout 1: \n" + render(lay1))
    //println("Rendered demo Layout 2: \n" + render(lay2))

    //println("the best layout for Doc: \n" + render(findBestLayout(document,2)))
  }

  "render" should "give an formated string as output" in {

    val tree:Tree=parseAE("if 3 == 5 then 3 else 4")
    val bigTree:Tree=parseAE("if 3 + 4 + 5 == 12 then 4 + 8 * 4 else 4 * 3")
    //val hugheTree:Tree=parseAE("if 1 + 1 == 2 then if 2 + 2 == 5 then 1111 + 222 + 33 + 4 else 4444 * 333 * 22 * 1 else if 1 == 2 then 2 + 2 else 4 * 5")
   // println("Enumerated Doc: " + enumerate(tree))
    val doc = enumerate(tree)



    println("Pretty printing at linewith 13: \n " + makeItPretty(tree,13))
    println("Pretty printing at linewith 40: \n " + makeItPretty(tree,40))
    println("Pretty printing at linewith 3: \n " + makeItPretty(tree,6)) //min is 6
    println("Print a big tree with 13: \n"+ makeItPretty(bigTree,13))
    println("Print a big tree with 40: \n"+ makeItPretty(bigTree,40))
    println("Print a big tree with 4: \n"+ makeItPretty(bigTree,4))
    //println("Print a hughe Tree with 40 lines: \n" + makeItPretty(hugheTree,40)) // hughe tree runs into stack overflow :(
  }





}
