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

  val plus = Comment(keywordParser(" + "))
  val dot = Comment(keywordParser(" * "))

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
  * Excercise 4./5.  I relieved Error: java.lang.StackOverflowError for a grammar like this. To fix this we have to eliminate the
  * left-recursion
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
        exp -> (num ~ add | num ~ mul | num),
        add -> (plus ~ add | num ~ add | num),
        mul -> (dot ~ mul | num ~ mul | num)))
  /*
  Resulting Tree:
  * Branch:
- 'exp
- List:
| - Leaf:
| | - 'num
| | - 1
| - Branch:
| | - 'add
| | - List:
| | | - Leaf:
| | | | - 'keyword
| | | | -  +
| | | - Branch:
| | | | - 'add
| | | | - List:
| | | | | - Leaf:
| | | | | | - 'num
| | | | | | - 2
  *
  *Exercise 7. My parser can not say in which branch he will descent. So add cannot stand as the top branch
  *
  * after setting plus and mul as an Terminal of type 'Comment'
  * Branch:
- 'exp
- List:
| - Leaf:
| | - 'num
| | - 1
| - Branch:
| | - 'add
| | - List:
| | | - Branch:
| | | | - 'add
| | | | - List:
| | | | | - Leaf:
| | | | | | - 'num
| | | | | | - 2
  * */


  /*end grammar*/

  /*grammar parsing*/
  def parseGrammar(grammar: Grammar): String => Tree = input => parseNonterminal(grammar.start, grammar)(input) match {

    case Some((exp, rest)) if rest.isEmpty =>

      //println(exp)
      println(simplifyTree(exp).treeString)
      simplifyTree(exp) //simplify the tree


    case Some((exp, rest)) if rest.nonEmpty =>
      sys.error("not an expression: " + input)

    case None =>
      sys.error("not an expression: " + input)
  }

  def parseNonterminal(nonterminal: Nonterminal, grammar: Grammar): Parser[Tree] =
    parseRHS(grammar lookup nonterminal, grammar) ^^ {

      grammar.lookup(nonterminal) match {
        /*
        case sel: Select =>
          println("Select")
          children => children(0)
*/

        case _ => children =>
          //println(nonterminal.symbol)
          Branch(nonterminal.symbol, children)
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

  /*simplify*/
  def simplifyTree(syntaxTree: Tree): Tree =
    syntaxTree match {
      case branch: Branch =>
        branch.symbol match {
          case 'exp => {

            val expChildren = branch.children
            if (branch.children.count(p => p.isInstanceOf[Tree]) > 1) {
              branch.children(1) match {
                case branch: Branch => {
                  //println("simbol: " + branch.symbol)
                  branch.symbol match {

                    case 'add => Branch('add, expChildren.map(simplifyTree))
                    case 'mul => Branch('mul, expChildren.map(simplifyTree))
                  }
                }
                case leaf: Leaf => {
                  leaf
                }
              }
            } else {
              simplifyTree(branch.children(0))

            }


          }

          case 'add => {

            //println("Amount of children: " + branch.children.count(p => p.isInstanceOf[Tree]))
            if (branch.children.count(p => p.isInstanceOf[Tree]) > 1) {
              Branch('add, branch.children.map(simplifyTree))
            } else {
              simplifyTree(branch.children(0))
            }


          }
          case 'mul => {

            if (branch.children.count(p => p.isInstanceOf[Tree]) > 1) {
              Branch('mul, branch.children.map(simplifyTree))
            } else {
              simplifyTree(branch.children(0))
            }

          }

        }


      case leaf: Leaf =>
        leaf.symbol match {
          case 'keyword =>
            leaf
          case 'num =>
            Leaf('num, leaf.code)
        }
    }

  /*after simplify eval can process it */
  def eval(t: Tree): Int = t match {
    case Branch('add, List(lhs, rhs)) =>
      eval(lhs) + eval(rhs)

    case Branch('mul, List(lhs, rhs)) =>
      eval(lhs) * eval(rhs)

    case Leaf('num, code) =>
      code.toInt
  }

  def parseAndEval(code: String): Int =
    eval(parseAE(code))


  /*
  * Excercise 8. I could not parse 2 + 3 * 4 because my grammar does not support this combination
  * But I think that, it will give me the wrong answer because my parser does not know about the rule of multiplication and addition
  *
  * */

}
