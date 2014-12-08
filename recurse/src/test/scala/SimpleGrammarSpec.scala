import org.scalatest._

import SimpleGrammar._

class SimpleGrammarSpec extends FlatSpec {
  it should "parse and simplify arithmetic expressions" in {
    //assert(parseAE("1234") == Leaf('num, "1234"))

    assert(parseAE("1 + 2") ==
      Branch('add, List(
        Leaf('num, "1"),
        Leaf('num, "2"))))

    assert(parseAE("1 * 2") ==
      Branch('mul, List(
        Leaf('num, "1"),
        Leaf('num, "2"))))

  }

  it should "parse a chain of a expressions" in {
    assert(parseAE("1 + 2 + 3") ==
      Branch('add, List(
        Leaf('num, "1"),
        Branch('add,List(
          Leaf('num, "2"),
          Leaf('num, "3"))) ))

    )

  }

  "parseAndEval" should "parse and eval arit. exoressions" in {

    assert(parseAndEval("1 + 2") == 3)
    assert(parseAndEval("1234") == 1234)
    assert(parseAndEval("3 * 5") == 15)
  }

  "parseAndEval" should "parse like in ex 8 described" in{
    assert(parseAndEval("2 + 3 * 4") == 14)
    assert(parseAndEval("2 * 3 + 4") == 10 )
  }


}
