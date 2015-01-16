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
    parseGrammar(ae)(code) //change used ae here


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





  case class Grammar(start: Nonterminal, rules: Map[Nonterminal, RuleRHS]) {
    def lookup(nonterminal: Nonterminal): RuleRHS = rules(nonterminal)
  }

  /*RuleRHS Objects*/
  val exp = Nonterminal('exp)
  val add = Nonterminal('add)
  val mul = Nonterminal('mul)
  val sub = Nonterminal('sub)
  val div = Nonterminal('div)

  val prio1 = Nonterminal('prio1)
  val prio2 = Nonterminal('prio2)

  val num = Terminal(digitsParser('num))


  val plus = Comment(keywordParser(" + "))
  val dot = Comment(keywordParser(" * "))
  val minus = Comment(keywordParser(" - "))
  val slash = Comment(keywordParser(" / "))

  def digitsParser(symbol: Symbol): Parser[Tree] =
    parseRegex("[0-9]+") ^^ { x => Leaf(symbol, x)}

  def keywordParser(keyword: String): Parser[Tree] =
    parseString(keyword) ^^ { x => Leaf('keyword, keyword)}

  /*start Grammar*/

/*Operator precedence and associativity */

  val ae: Grammar =
    Grammar(
      start = exp,
      rules = Map(
        exp -> (prio1 ~ add | prio1 ~ sub | num ~ mul | num ~ div | num),
        prio1-> (num ~ mul | num ~ div | num ),
        add -> (plus ~ exp),
        sub -> (minus ~ exp),
        mul -> (dot ~ prio1 ),
        div -> (slash ~ prio1)))


  /*
  val ae: Grammar =
    Grammar(
      start = exp,
      rules = Map(
        exp -> (prio2 ~ add | prio1 ~ sub | num ~ mul | num ~ div | num),
        prio1-> (num ~ mul | num ~ div | num ),
        prio2-> (prio1 ~ sub | prio1  ),
        add -> (plus ~ exp),
        sub -> (minus ~ prio2),
        mul -> (dot ~ prio1 ),
        div -> (slash ~ prio1)))
*/
  /*end grammar*/

  /*grammar parsing*/
  def parseGrammar(grammar: Grammar): String => Tree = input => parseNonterminal(grammar.start, grammar)(input) match {

    case Some((exp, rest)) if rest.isEmpty =>
      //println("Raw Tree: " + exp.treeString)
      println("SimplyfiedTree: " + simplifyTree(exp).treeString)
      println("Handled Tree: " + handleSubDiv( simplifyTree(exp)).treeString )
      handleSubDiv(simplifyTree(exp)) //simplify the tree


    case Some((exp, rest)) if rest.nonEmpty =>
      println("Demaged Tree:")
      println(exp.treeString)
      sys.error("not an expression: " + input)

    case None =>
      println("empty")
      sys.error("not an expression: " + input)
  }

  def parseNonterminal(nonterminal: Nonterminal, grammar: Grammar): Parser[Tree] =
    parseRHS(grammar lookup nonterminal, grammar) ^^ {

      grammar.lookup(nonterminal) match {

        case _ => children =>
          //println(nonterminal.symbol)
          Branch(nonterminal.symbol, children)
      }
    }


  def parseRHS(ruleRHS: RuleRHS, grammar: Grammar): Parser[List[Tree]] =
    ruleRHS match {
      case nonterminal: Nonterminal =>
        //println("Nonterminal: " + nonterminal.symbol)
        //parseRHS(grammar lookup nonterminal, grammar) //call parseRHS again until you reach a terminal
        parseNonterminal(nonterminal, grammar) ^^ {
          case (parserOfNonterminal) =>
            List(parserOfNonterminal)
        }
      case terminal: Terminal =>
        //println ("Terminal(Num)")
        terminal.parse ^^ {
          case (parserOfTree) =>
            List(parserOfTree)
        }
      case comment: Comment => //do not add to tree
        //println("Comment")
        comment.parse ^^ {
          case someComment =>
            List.empty //return no list
        }
      case seq: Sequence =>
        //println ("Sequenz!")
        parseRHS(seq.lhs, grammar) ~ parseRHS(seq.rhs, grammar) ^^ {
          case (lhs, rhs) =>
            lhs ::: rhs
        }
      case sel: Select =>
        //println("Selection!")
        parseRHS(sel.lhs, grammar) | parseRHS(sel.rhs, grammar) ^^ {
          case (result) =>
            result //never return result from select
        }
    }

  /*simplify*/
  def simplifyTree(syntaxTree: Tree): Tree =
    syntaxTree match {
      case branch: Branch =>
        //println("Number of Branch children: " + branch.children.count(p => p.isInstanceOf[Tree]))//max 2 children
        branch.symbol match {
          case 'exp => {

            val expChildren = branch.children
            if (branch.children.count(p => p.isInstanceOf[Tree]) > 1) {

              //println("branch children" + branch.children)
              branch.children(1) match {
                case branch: Branch => {
                  //println("simbol: " + branch.symbol)
                  Branch(branch.symbol, expChildren.map(simplifyTree))

                }
                case leaf: Leaf => {
                  leaf
                }

              }

            } else {
              simplifyTree(branch.children(0))

            }


          }
          case 'prio1 =>{
            val prio1Children = branch.children
            if (branch.children.count(p => p.isInstanceOf[Tree]) > 1) {

              //println("branch children" + branch.children)
              branch.children(1) match {
                case branch: Branch => {
                  //println("simbol: " + branch.symbol)
                  Branch(branch.symbol, prio1Children.map(simplifyTree))
                }
                case leaf: Leaf => {
                  leaf
                }
              }
            } else {
              simplifyTree(branch.children(0))
            }
          }
          case 'prio2 =>{
            val prio2Children = branch.children
            if (branch.children.count(p => p.isInstanceOf[Tree]) > 1) {

              //println("branch children" + branch.children)
              branch.children(1) match {
                case branch: Branch => {
                  Branch(branch.symbol, prio2Children.map(simplifyTree))
                }
                case leaf: Leaf => {
                  leaf
                }
              }
            } else {
              simplifyTree(branch.children(0))
            }
          }
          case _ => {
            if (branch.children.count(p => p.isInstanceOf[Tree]) > 1 ){
              Branch(branch.symbol, branch.children.map(simplifyTree))
            }
            else  {
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
var hasSubAsParent:Boolean = false

  def handleSubDiv(syntaxTree: Tree): Tree ={
   syntaxTree match{
     case branch:Branch=>
       val leftChild = branch.children(0)
       val rightChild = branch.children(1)
       branch.symbol match {
         case 'sub | 'div => {
           //current branch is a subtraction
           //check for children
           if (branch.children.count(p => p.isInstanceOf[Tree]) > 1) {
             //if its child is an subtraction too -> left rotate
             val rightChild = branch.children(1)
             rightChild match{
               case rCB:Branch =>
                 rCB.symbol match{//test if it is an subtraction
                   case 'sub | 'add | 'div => //perform right rotation

                     val newLeftChild = Branch(branch.symbol, List(branch.children(0), rCB.children(0)))
                     val newRightChild = rCB.children(1)

                     handleSubDiv( Branch(rCB.symbol,List(newLeftChild,newRightChild)))
                   case _ => // Do not left rotate here

                     Branch(branch.symbol,branch.children.map(handleSubDiv)) //do nothing
                 }
               case rCL:Leaf =>
                 Branch(branch.symbol,branch.children.map(handleSubDiv)) //do nothing
             }


           }else{
             //just give back this tree
             handleSubDiv(branch.children(0))
           }

         }


           /*All other branch types*/
         case _ => {

           if (branch.children.count(p => p.isInstanceOf[Tree]) > 1 ){
            Branch(branch.symbol, branch.children.map(handleSubDiv))
           }
           else  {
             handleSubDiv(branch.children(0))
           }
         }
       }
     case leaf:Leaf=>
       leaf
   }

  }


  /*after simplify eval can process it */
  def eval(t: Tree): Int = t match {
    case Branch('sub, List(lhs, rhs)) =>
      eval(lhs) - eval(rhs)

    case Branch('div, List(lhs, rhs)) =>
      eval(lhs) / eval(rhs)

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
* Task 2:
* It does not parse 5 - 2 - 1 and 32 / 4 / 2 correctly, because the tree structure
 * leads the parser to solve always the right hand side of the expression first.
  * So it calculates the term like "5- (2-1)" and "32 / (4/2)"
* */

}
