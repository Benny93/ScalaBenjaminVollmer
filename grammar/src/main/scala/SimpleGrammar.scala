/** Exercise 3.3: A more sophisticated interpreter of grammars
  *
  * In 3.2, you implemented the method `simplifyAE` so that
  * the result of parsing the grammar `ae` is easier to use.
  * We will now make the simplicification work for all grammars.
  *
  * Tasks:
  *
  * 1. Design a data structure for grammars and implement
  *    a grammar interpreter so that users need not write
  *    a simplifying method like `simplifyAE` for every grammar
  *    they define. Instead, the parser of the grammar should
  *    always produce simplified syntax trees.
  *
  * 2. Create a grammar object. You may choose to either write
  *    something equivalent to `NaiveGrammar.ae` in 3.2, or
  *    describe arithmetic expressions with arbitrary spacing
  *    between words (ex. 2.2).
  *
  * 3. Test that your grammar interpreter works as expected.
  *
  *
  * ===================== SPOILER BEGINS =====================
  * You may want to have grammar objects contain information
  * about how to simplify their syntax trees.
  *
  * 1. Instead of `Terminal`, keywords could have their own case
  *    class (say, `Comment`), and the grammar interpreter could
  *    discard all syntax tree nodes created from them.
  *
  * 2. Instead of `Choice`, exp could be defined in terms of a
  *    new case class (say, `Select`). The grammar interpreter
  *    never creates new syntax tree nodes from `Select`.
  * ====================== SPOILER ENDS ======================
  */

object SimpleGrammar extends util.Combinators {
  sealed trait Tree
  case class Leaf(symbol: Symbol, code: String) extends Tree
  case class Branch(symbol: Symbol, children: List[Tree]) extends Tree


  /** Parsing the grammar of your choice.
    * Always produce simplified syntax trees.
    * Should not be hard-coded for arithmetic expressions.
    */
  def parseAE(code: String): Tree =
  parseGrammar(ae)(code)

  /*implmentation*/
  case class Select(lhs: RuleRHS, rhs: RuleRHS) extends RuleRHS //the grammar interpreter should never create tree note from this
  case class Sequence(lhs: RuleRHS, rhs: RuleRHS) extends RuleRHS

  sealed trait RuleRHS { //sealed trat means, that the number of possible matches is not infinite
  def |(rhs: RuleRHS) = Select(this, rhs)
    def ~(rhs: RuleRHS) = Sequence(this, rhs)
  }

  case class Nonterminal(symbol: Symbol) extends RuleRHS
  case class Terminal(parse: Parser[Tree]) extends RuleRHS
  case class Comment(parse: Parser[Tree]) extends RuleRHS //for keywords that should be ignored

  /*Test Grammar*/

  case class Grammar(start: Nonterminal, rules: Map[Nonterminal, RuleRHS]) {
    def lookup(nonterminal: Nonterminal): RuleRHS = rules(nonterminal)
  }

  /*RuleRHS Objects*/
  val exp = Nonterminal('exp)
  val add = Nonterminal('add)
  val mul = Nonterminal('mul)

  val num = Terminal(digitsParser('num))
  val sumOf = Comment(keywordParser("sum of "))
  val productOf = Comment(keywordParser("product of "))
  val and = Comment(keywordParser(" and "))

  def digitsParser(symbol: Symbol): Parser[Tree] =
    parseRegex("[0-9]+") ^^ { x => Leaf(symbol, x) }

  def keywordParser(keyword: String): Parser[Tree] =
    parseString(keyword) ^^ { x => Leaf('keyword, keyword) }

  val ae: Grammar =
    Grammar(
      start = exp,
      rules = Map(
        exp -> (add | mul | num),
        add -> (sumOf ~ exp ~ and ~ exp),
        mul -> (productOf ~ exp ~ and ~ exp)))
  /*end grammar*/

  /*grammar parsing*/
  def parseGrammar(grammar: Grammar): String => Tree = input => parseNonterminal(grammar.start, grammar)(input) match {

    case Some((exp, rest)) if rest.isEmpty =>
      exp

    case Some((exp, rest)) if rest.nonEmpty =>
      sys.error("not an expression: " + input)

    case None =>
      sys.error("not an expression: " + input)
  }
  def parseNonterminal(nonterminal: Nonterminal, grammar: Grammar): Parser[Tree] =
    parseRHS(grammar lookup nonterminal, grammar) ^^ {
      children => Branch(nonterminal.symbol, children)
    }

  def parseRHS(ruleRHS: RuleRHS, grammar: Grammar): Parser[List[Tree]] =
    ruleRHS match {
      case nonterminal:Nonterminal =>
        parseRHS(grammar lookup nonterminal, grammar) //call parseRHS again until you reach a terminal
      case terminal:Terminal=>
        terminal.parse ^^{
          case (parserOfTree)=>
            List(parserOfTree)
        }
      case comment:Comment=> //do not add to tree
        comment.parse ^^{
          case someComment =>
          List.empty  //return no list
        }
      case seq:Sequence=>
        parseRHS(seq.lhs,grammar) ~ parseRHS(seq.rhs,grammar) ^^{
          case (lhs,rhs)=>
            lhs ::: rhs
        }
      case sel:Select=>
        parseRHS(sel.lhs,grammar) | parseRHS(sel.rhs,grammar) ^^{
          case (result)=>
            ??? //never return result from select
        }
    }


}
