import org.scalatest._

import SimpleGrammar._

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

  "render" should "give an formated string as output" in {
    println("|Hallo|" + addWhitespaces("Hallo",10))
    val lay : Layout = {
      List((0,"Hallo"),(2,"World!"))
    }
    println("rendered Layout: \n" + render(lay))
  }


}
