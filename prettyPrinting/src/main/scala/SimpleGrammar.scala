/**
 * Pretty Printing Benjamin Vollmer
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

  //current in use:
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
      println("Damaged Tree:")
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

/*UNPARSING A TREE*/
  def unparse(tree: Tree): String  = {
  tree match{
    case branch:Branch=>
      branch.symbol match{
        case 'if => //because i know the structure of the tree, i can leave children hard coded
          "if " + unparse(branch.children(0)) + " then " + unparse(branch.children(1)) + " else " + unparse(branch.children(2))
        case 'add =>
          unparse(branch.children(0)) + " + " + unparse(branch.children(1)) //add has 2 children
        case 'sub =>
          unparse(branch.children(0)) + " - " + unparse(branch.children(1))

        case 'mul => unparse(branch.children(0)) + " * " + unparse(branch.children(1))
        case 'div => unparse(branch.children(0)) + " / " + unparse(branch.children(1))

        case 'eq =>
          unparse(branch.children(0)) + " == " + unparse(branch.children(1))

      }

    case leaf:Leaf=>
      leaf.code //leaf is now only of type num and only contains numbers as strings
  }
}//end unparse

  type Layout = List[(Int, String)]
  def Layout(xs:(Int,String)*) = List(xs: _*)
  type Doc = List[Layout]
  def Doc(xs: Layout*) = List(xs: _*)


  def makeItPretty(tree: Tree, lineWidth: Int): String = {
    render(findBestLayout(enumerate(tree),lineWidth))
  }


  def render(layout: Layout):String ={
    var resString:String = ""
    layout.foreach(singleLayout =>{
     resString += addWhitespaces(singleLayout._2,singleLayout._1) + "\n"
    })
   return resString
  }


  def addWhitespaces(string:String, amount:Int):String = {
    var input= string
    if (amount > 0) {

      input = " " + input
      addWhitespaces(input, amount - 1)
    }else {
      return input
    }
  }


  // step 1: enumerate all possible ways to print a syntax tree
  def enumerate(tree: Tree): Doc = {
    tree match {
      case branch:Branch=>
        branch.symbol match{
          case 'if=>
            combineDocuments(List( List(List((0,"if "))),enumerate(branch.children(0)), List(List((0," then "))),enumerate(branch.children(1)), List(List((0," else "))),enumerate(branch.children(2))))
          /*equal*/
          case 'eq=>
            combineDocuments(List(enumerate(branch.children(0)), List(List((0," == "))),enumerate(branch.children(1))))
          /*mul and div*/
          case 'mul =>
            combineDocuments(List(enumerate(branch.children(0)), List(List((0," * "))),enumerate(branch.children(1))))
          case 'div=>
            combineDocuments(List(enumerate(branch.children(0)), List(List((0," / "))),enumerate(branch.children(1))))

          /*addition and subtraction*/
          case 'add=>
            combineDocuments(List(enumerate(branch.children(0)), List(List((0," + "))),enumerate(branch.children(1))))
          case 'sub=>

            combineDocuments(List(enumerate(branch.children(0)), List(List((0," - "))),enumerate(branch.children(1))))
        }

      case leaf:Leaf=>
        //leaf.code //always a number
        val lay:Layout={List((0,leaf.code))}
        val docForLeaf:Doc ={List(lay)}
        return docForLeaf
    }

  }




  def combineDocuments(docs:List[Doc]):Doc = {
    docs match {
      case List(lhs, operator, rhs) => {

        for {//for every layout
          lLay <- lhs
          rLay <- rhs
          oLay <- operator
          breakLine <- List(1, 2, 3)
        } yield {
          //for every content of the layout
          if (breakLine == 1) {
            lLay ++ oLay ++ rLay
          } else {
            if (breakLine == 2) {
              mergeTwoLayouts(lLay, oLay) ++ addSomeIndentToLayout(rLay, 2)
            } else {
              mergeTwoLayouts(mergeTwoLayouts(lLay, oLay), rLay)
            }
          }
        }

      }
      case List(ifOpDoc, conditionDoc, thenDoc, result1Doc, elseDoc, result2Doc) => {

        for {//for every layout
          ifs <- ifOpDoc
          //cons <- conditionDoc
          thenLay <- thenDoc
          result1 <- result1Doc
          elseLay <- elseDoc
          result2 <- result2Doc
          breakLine <- List(1, 2, 3)
        } yield {
          //for every content of the layout
          if (breakLine == 1) {
            ifs ++ addSomeIndentToLayout(conditionDoc(2),2) ++ thenLay ++ addSomeIndentToLayout( result1,2) ++
              elseLay ++ addSomeIndentToLayout(result2, 2)
          } else {
            if (breakLine == 2) {
              mergeTwoLayouts(ifs, conditionDoc(2)) ++ addSomeIndentToLayout(thenLay, 3) ++
                addSomeIndentToLayout(result1, 2) ++ elseLay ++ addSomeIndentToLayout(result2, 2)
            } else {
              mergeTwoLayouts(mergeTwoLayouts(ifs, conditionDoc(2)), thenLay) ++ addSomeIndentToLayout(result1, 2) ++
                elseLay ++ addSomeIndentToLayout(result2, 2)
            }
          }
        }
      }
    }
  }


  def mergeTwoLayouts(layout1:Layout, layout2: Layout):Layout={ //merge two layouts. the indent of the first layout will be taken
    for{
      l1 <- layout1
      l2 <-layout2
    }yield{
      (l1._1,l1._2 + l2._2 )
    }
  }

  def addSomeIndentToLayout(layout: Layout, indent: Int):Layout={
    for {
      lay <-layout
    }yield{
      (lay._1 + indent ,lay._2)
    }
  }

/* //This is not in use but maybe should be in use
  def mergeConditionLayout(condition:Layout): Layout ={ // !! attention if condition is longer than line with??? //Layout ist List[Int,String]

    for{
      lay <- condition
    }yield{
      (lay._1,lay._2)
    }

  }
*/

  // step 2: find the best layout according to some line width
  def findBestLayout(doc: Doc, lineWidth: Int): Layout={
    var fits:Boolean = true
    var anyLayout:Layout={List()}
    doc.foreach(singleLayout =>{

      singleLayout.foreach(layoutPart=>{
        if((layoutPart._1.toString + layoutPart._2).toString.length > lineWidth){
          fits = false
        }
      })
      if(fits){
        anyLayout = singleLayout
      }else{
        fits = true // continue trying
      }
    })
    return anyLayout // if no layout fits give back the last one. -> in future this will give back the smallest one
  }

}
