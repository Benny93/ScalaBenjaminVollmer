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
    parseGrammar(aeBig)(code) //change used ae here


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
  val prio3 = Nonterminal('prio3)
  val equals = Nonterminal ('eq)
  val ifExp = Nonterminal('if)
  val num = Terminal(digitsParser('num))


  val plus = Comment(keywordParser(" + "))
  val dot = Comment(keywordParser(" * "))
  val minus = Comment(keywordParser(" - "))
  val slash = Comment(keywordParser(" / "))
  val eqSgn = Comment(keywordParser(" == "))
  val ifKw = Comment(keywordParser("if "))
  val thenKw = Comment(keywordParser(" then "))
  val elseKw = Comment(keywordParser(" else "))

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

  val aeBig: Grammar =
    Grammar(start = exp,
      rules = Map(
        exp -> (ifExp | equals | prio2),
        ifExp -> (ifKw ~ equals ~ thenKw ~ exp ~ elseKw ~ exp ),
        equals -> (prio2 ~ eqSgn ~ prio2),
        prio2 ->(prio1 ~ add | prio1 ~ sub | num ~ mul | num ~ div | num), //
        prio1-> (num ~ mul | num ~ div | num ),
        add -> (plus ~ prio2),
        sub -> (minus ~ prio2),
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

          case 'exp | 'prio1 | 'prio2 | 'prio3 =>{
            val branchChildren = branch.children
            if (branch.children.count(p => p.isInstanceOf[Tree]) > 1) {

              branch.children(1) match {
                case branch: Branch => {
                  Branch(branch.symbol, branchChildren.map(simplifyTree))
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

                     if (branch.symbol == 'sub && rCB.symbol == 'div) {
                       //Do not left rotate
                       Branch(branch.symbol,branch.children.map(handleSubDiv))
                     }else{
                       //right rotate
                       val newLeftChild = Branch(branch.symbol, List(branch.children(0), rCB.children(0)))
                       val newRightChild = rCB.children(1)

                       handleSubDiv(Branch(rCB.symbol, List(newLeftChild, newRightChild)))
                     }

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

    case Branch('eq, List(lhs,rhs))=> { //Equality
      if (eval(lhs) == eval(rhs)) {
        return 1
      } else {
        return 0
      }
    }
    case Branch('if, List(condition,ifTrue,ifFalse))=>{
      if(eval(condition) == 1){
        eval(ifTrue)
      }else{
        eval(ifFalse)
      }
    }

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
