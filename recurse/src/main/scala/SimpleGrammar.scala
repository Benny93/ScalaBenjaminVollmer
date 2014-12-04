/**
 * old class From Exercise 3.3: A more sophisticated interpreter of grammars reused
 *
 *
 */

import sext._

object SimpleGrammar extends util.Combinators {



  sealed trait Tree

  case class Leaf(symbol: Symbol, code: String) extends Tree

  case class Branch(symbol: Symbol, children: List[Tree]) extends Tree


  def parseAE(code: String): Tree =
    parseGrammar(ae)(code)

  /*implmentation*/
  case class Select(lhs: RuleRHS, rhs: RuleRHS) extends RuleRHS

  //the grammar interpreter should never create tree note from this
  case class Sequence(lhs: RuleRHS, rhs: RuleRHS) extends RuleRHS

  sealed trait RuleRHS {
    //sealed trat means, that the number of possible matches is not infinite
    def |(rhs: RuleRHS) = Select(this, rhs)

    def ~(rhs: RuleRHS) = Sequence(this, rhs)
  }

  case class Nonterminal(symbol: Symbol) extends RuleRHS

  case class Terminal(parse: Parser[Tree]) extends RuleRHS

  case class Comment(parse: Parser[Tree]) extends RuleRHS

  //for keywords that should be ignored

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

  val plus = Terminal(keywordParser(" + "))
  val dot = Terminal(keywordParser(" * "))

  def digitsParser(symbol: Symbol): Parser[Tree] =
    parseRegex("[0-9]+") ^^ { x => Leaf(symbol, x)}

  def keywordParser(keyword: String): Parser[Tree] =
    parseString(keyword) ^^ { x => Leaf('keyword, keyword)}

  /*new Grammar*/

  /*
 * Exp := Num | Add | Mul
 * Num := <a natural number, just like before>
 * Add := Exp + Exp
 * Mul := Exp * Exp
 *
 * */

  /*parsing left recursive Grammar does not work, because it will end up in an infinite loop.
  * Convert to right recursion first!
  *
  * 4./5.  I relieved Error: java.lang.StackOverflowError for a grammar like this
  *
  * val ae: Grammar =
    Grammar(
      start = exp,
      rules = Map(
        exp -> (add | mul | num),
        add -> (exp ~ plus ~ exp),
        mul -> (exp ~ dot ~ exp)))
  * */

  /*solution -->*/
   val ae: Grammar =
    Grammar(
      start = exp,
      rules = Map(
        exp -> (add | mul | num),
        add -> (exp ~ plus ~ exp),
        mul -> (exp ~ dot ~ exp)))

  /*end grammar*/

  /*grammar parsing*/
  def parseGrammar(grammar: Grammar): String => Tree = input => parseNonterminal(grammar.start, grammar)(input) match {

    case Some((exp, rest)) if rest.isEmpty =>
    println(exp.treeString)
      exp

    case Some((exp, rest)) if rest.nonEmpty =>
      sys.error("not an expression: " + input)

    case None =>
      sys.error("not an expression: " + input)
  }

  def parseNonterminal(nonterminal: Nonterminal, grammar: Grammar): Parser[Tree] =
    parseRHS(grammar lookup nonterminal, grammar) ^^ {

      grammar.lookup(nonterminal) match {
        case sel: Select =>
          children => children(0)


        case _ => children => Branch(nonterminal.symbol, children)
      }
    }


  def parseRHS(ruleRHS: RuleRHS, grammar: Grammar): Parser[List[Tree]] =
    ruleRHS match {
      case nonterminal: Nonterminal =>
        //parseRHS(grammar lookup nonterminal, grammar) //call parseRHS again until you reach a terminal
        parseNonterminal(nonterminal, grammar) ^^ {
          case (parserOfNonterminal) =>
            List(parserOfNonterminal)
        }
      case terminal: Terminal =>
        terminal.parse ^^ {
          case (parserOfTree) =>
            List(parserOfTree)
        }
      case comment: Comment => //do not add to tree
        comment.parse ^^ {
          case someComment =>
            List.empty //return no list
        }
      case seq: Sequence =>
        parseRHS(seq.lhs, grammar) ~ parseRHS(seq.rhs, grammar) ^^ {
          case (lhs, rhs) =>
            lhs ::: rhs
        }
      case sel: Select =>
        parseRHS(sel.lhs, grammar) | parseRHS(sel.rhs, grammar) ^^ {
          case (result) =>
            result //never return result from select
        }
    }


}
