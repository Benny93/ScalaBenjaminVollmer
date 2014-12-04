import org.scalatest._

import SimpleGrammar._

class SimpleGrammarSpec extends FlatSpec {
  it should "parse and simplify arithmetic expressions" in {
    //assert(parseAE("1234") == Leaf('num, "1234"))

    assert(parseAE("1 + 2") ==
      Branch('add, List(
        Leaf('num, "1"),
        Leaf('num, "2"))))

  }


}
