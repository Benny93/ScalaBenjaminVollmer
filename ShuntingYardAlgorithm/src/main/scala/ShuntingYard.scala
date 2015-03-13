/**
 * Created by Benny on 01.03.15.
 * Final Project
 */

import sext._

import scala.collection.immutable.Queue

object ShuntingYard extends util.Combinators {

  /*
  * In this final project Dijkstra's shunting yard algorithm will be implemented.
  *
  * Basically in ShuntingYard we want to go from infix notation to reverse polish.
  *
  * infix notation: Human readable default expression like i = 9+24/(7-3).
  * reverse polish: Operations are set in the order they must be performed.
  *
  * The elements working in this Algorithm are:
  *   +Token: Every Part of the input string, like a number or an operator.
  *   +Operator: Any mathematical symbol used to represent an operation. That way +,-,/ and * are all operators. "(" brackets are not operators!
  *   +Stack: A data structure with LiFo data order. This will be used for operators
  *   +Queue: A data structure with FiFo data order. This will be used for the output
  *   +List or Array: A data structure where every element can be accessed by its index position. This will be used for the tokens
  */

  /*
  * Rules for Reverse Polish (this is not the shunting yard algorithm yet):
  *   -Expressions are parsed from left to right
  *   -Every number or operator that is read will be pushed to a stack
  *   -Each time an operator is read, we pop the required operands from stack, perform the operation and push the
  *     result back to the stack.
  *   -We are finished when there is no more tokens to read. The final number in the stack is the result.
  *
  * So the expression i looks in reverse polish like: 9, 24 , 7 , 3 , - , / , +
  *
  * Reading reverse polish on this will work like:
  *
  * -The numbers added to the stack in the order
  * Stack: [3,7,24,9] where 9 was pushed in first and 3 was pushed in last
  *
  * -Then the operators are read
  * Stack: [4,24,9] because "-" required 2 operants we pop 3 and 7 from stack. After the operation we
  *   push the result (which is 4) back to the stack. After that "/" proceeds
  *
  * -This continues for every operator
  * Stack:[6,9] "+" needs two operands 6 and 9
  *
  * Stack:[15] the last entry in the stack is our result
  *
  * It is important for the algorithm to know, how many operands one operation needs.
  *
  *
  * Shunting Yard Algorithm can get very big and very blew up, because there are many details to work on.
  */

  /*
  * The basic shunting yard algorithm:
  * For all Tokens of the list (in the right order of infix)
  * |  read a token t
  * |  if( t is a number)=> add t to output QUEUE
  * |  if(t is an operator)
  * |   |   while(there is an operator j ON TOP of STACK with greater or equal persistence than t)
  * |   |   | push j to output QUEUE
  * |   |   end while
  * |   |   push t to STACK
  * |   end if
  * |   if( t is a LEFT bracket "(")=> push t to STACK
  * |   if( t is a RIGHT bracket ")")
  * |   |   while(the operator on TOP of STACK j is not a left bracket)
  * |   |   | pop operators form STACK to QUEUE
  * |   |   pop the left bracket "(" and discard it
  * |   end if
  * end for all
  * while(there are operators on STACK)
  * | pop operators to output QUEUE
  * end while
  *
  * The Output QUEUE now contains all the tokens in reverse polish order! (the second step is to translate that polish order)
  *
  * For things like If-then-Else this algorithm has to be advanced as i will show later.
  *
  * Problems to avoid:
  * If the tokens on the STACK ran out while searching for the left outer bracket, there is a bracket mismatch.As an
  * example: "5+6)" or "(5+6) + 5)". Here the right bracket has no left bracket to match.
  *
  *Also if the algorithm finds left brackets whilst it is pooping the final operators. We have unmatched left brackets like in
  * "(5+6" or "((5+6)+3".
  *
  * Operators need a notion of associativity whether the have to stand left or right in the expression.
  * */

  //IMPLEMENTATION

  /*First i want to create all the necessary objects*/

  sealed trait Token

  case class Number(code: String) extends Token

  case class Bracket(code: String, isLeftBracket: Boolean) extends Token

  case class Operator(code: String, numberOfOperands: Byte, levelOfPersistence: Byte) extends Token

  case class Comment(code: String) extends Token



  //Parsing basics for tokenize
  def numberParser(): Parser[Token] =
    parseRegex("[0-9]+") ^^ { x => Number(x)}

  def operatorParser(operator: String, numberOfOperands: Byte, levelOfPersistence: Byte): Parser[Token] =
    parseString(operator) ^^ { x => Operator(x, numberOfOperands, levelOfPersistence)}

  def bracketParser(bracket: String): Parser[Token] =
    parseString(bracket) ^^ { x => Bracket(x, x == "(")}

  def whiteSpaceParser(ws: String): Parser[Token] =
    parseString(ws) ^^ { x => Comment(x)}

  def commentParser(com: String): Parser[Token] =
    parseString(com) ^^ { x => Comment(x)}

  val number = numberParser()
  val plus = operatorParser("+", 2, 2)
  val minus = operatorParser("-", 2, 3)
  val mul = operatorParser("*", 2, 4)
  val div = operatorParser("/", 2, 4)
  val ifCondition = operatorParser("if", 3, 0)
  val equals = operatorParser("==",2,1)
  val pow = operatorParser("^",2,5)
  val leftBracket = bracketParser("(")
  val rightBracket = bracketParser(")")
  val whitespace = whiteSpaceParser(" ")

  val thenKeyword = commentParser("then")
  val elseKeyword = commentParser("else")



  //Here are the needed parsers for the combinator. It uses the choice function to find the fitting token for each character.
  def chooseToken:Parser[Token] = tOperator | tBracket | tComment | number |  whitespace
  def tOperator:Parser[Token] = plus | minus | mul | div | ifCondition | equals | pow
  def tBracket:Parser[Token] = leftBracket | rightBracket
  def tComment:Parser[Token] = thenKeyword | elseKeyword

  def tokenChain(parser: => Parser[Token]):Parser[List[Token]] = {
    //tOperator | tBracket | tComment | number | whitespace
    input => parser(input) match{
      case None =>
        Some((List.empty, input))
      case Some((firstResults,afterFirstResult)) =>
        tokenChain(parser)(afterFirstResult) match{
          case Some((otherResults, afterOtherResults))=>
            Some((firstResults :: otherResults, afterOtherResults))
          case None =>
            None
        }
    }
  }


  //main function
  def parseAEWithShuntingYard(code: String): Int = {

    stack = List() //free variables, because they are public and therefore contain old content
    resultStack = List()

    val step1 = tokenizeExpression2(code)
    try {
    val step2 = convertToReversePolish(step1)
    val step3 = translateRPToResultStack(step2)
    return extractResultFromResultStack(step3)
    }catch{
      case e:Exception => println("Exceprtion: " + e + " not a valid Expression!!")
    }
    return 0
  }



  /*new implementation of tokenize*/
  /*I created a much shorter new implementation of tokenize, which is using combinators and works simmilar like an
  * zero or more parser.
  *
  * */

  //1. get all the tokens
  def tokenizeExpression2(code:String):List[Token]={
    tokenChain(chooseToken)(code) match {
      case Some((tokens,rest))if rest.isEmpty => tokens.filter(p =>{
        !p.isInstanceOf[Comment] || p.isInstanceOf[Comment] && !p.asInstanceOf[Comment].code.equals(" ") //Whitespaces should be ignored
        })
      case Some((tokens,rest)) if !rest.nonEmpty=>
        sys.error("Not an expression: " + code)
      case None => sys.error("Not an expression " + code)

    }
  }




  //Stack is an inelegant and potentially poorly-performing wrapper
  // around List.
  // Use List instead:
  // stack push x becomes x :: list;
  // stack.pop is list.tail.
  var stack = List[Token]()

  //2.convert to reverse polish
  def convertToReversePolish(tokenList: List[Token]): Queue[Token] = {
    if (tokenList.isEmpty) { //every token has been used
      //pop everything from stack to queue

      var queue = Queue[Token]() //buffer for new queue content

      if (!stack.isEmpty) {
        queue = queue.enqueue(stack.filter(opt =>{
        opt.isInstanceOf[Operator]  //do only push operator Token to output queue
        }))

        while (!stack.isEmpty) {
          stack = stack.tail
        }
      }

      return queue
    } else {

      tokenList.head match {
        case number: Number =>
          return Queue[Token]().enqueue(number) ++ convertToReversePolish(tokenList.tail) //just put it to output queue


        case cOpt: Operator => //Token is operator
          var outPut = Queue[Token]()
          if (!stack.isEmpty) {
            //if stack is not empty
            var otherToken: Token = stack.head //first other Token on stack
            if (otherToken.isInstanceOf[Operator]) {
              var otherOperator: Operator = otherToken.asInstanceOf[Operator]
              var stackIsEmpty: Boolean = false

              while (!stackIsEmpty && stack.head.isInstanceOf[Operator]
                && otherOperator.levelOfPersistence >= cOpt.levelOfPersistence && !cOpt.code.equals("if")) { //"if" is never allowed to push another operator away



                outPut = outPut.enqueue(otherOperator)
                //pop other
                stack = stack.tail
                //get new other Operator
                if (!stack.isEmpty) {
                  if (stack.head.isInstanceOf[Operator]) {
                    otherOperator = stack.head.asInstanceOf[Operator]
                  }
                } else {
                  stackIsEmpty = true
                }
              }
            }

          }

          //push current token to stack
          stack = cOpt :: stack

          val resultQ = outPut ++ convertToReversePolish(tokenList.tail) //concatenate outputQueue with future OutputQueue

          return resultQ

        case bracket: Bracket =>
          var outPut = Queue[Token]()
          if (bracket.isLeftBracket) {
            //push to stack
            stack = bracket :: stack
            return convertToReversePolish(tokenList.tail)

          } else {

            if (!stack.isEmpty) {
              //if stack is not empty
              var popOperator: Boolean = false
              var otherToken: Token = stack.head //first other Token on stack

              if (otherToken.isInstanceOf[Operator]) {
                popOperator = true
              } else {
                popOperator = false
              }

              //pop everything inside brackets ( ) to queue
              while (popOperator) {
                //
                outPut = outPut.enqueue(otherToken) //pop from stack to queue
                stack = stack.tail //pop first element

                otherToken = stack.head
                if (otherToken.isInstanceOf[Bracket]) {
                  // has to be left bracket because we never push right bracket to the stack
                  popOperator = false
                }

              }
              // pop left bracket -> has to be left bracket now because we popped everything before it
              if (otherToken.asInstanceOf[Bracket].isLeftBracket) {
                //pop left bracket and discard it
                stack = stack.tail
              }
            }

            return outPut ++ convertToReversePolish(tokenList.tail)
          }

        case com: Comment => //To read more about how this part works read underneath the function
          var outPut = Queue[Token]()
          var stackIsEmpty:Boolean = false
            /*CASE FOR THEN */
            if(com.code.equals("then")){
              //pop everything until if from stack, but keep if on the stack


              while (!stackIsEmpty && !stack.head.asInstanceOf[Operator].code.equals("if")){
                //push to queue
                outPut = outPut.enqueue(stack.head)
                //pop
                stack = stack.tail
                if(stack.isEmpty){stackIsEmpty = true} //stop is stack is empty
              }//stop if "if" has been reached
            }
            /*CASES FOR ELSE*/
            if(com.code.equals("else")){
              //this one is for nested if-then-else. It is important to know when on of these exp ends. So else is needed on stack
              if(stack.head.isInstanceOf[Comment] && stack.head.asInstanceOf[Comment].code.equals("else")){
                //push everything from stack to queue until a "if" operator has been reached.
                //"then" should not be pushed


                stack = stack.tail //first throw else away

                while(!stackIsEmpty && stack.head.isInstanceOf[Operator] && !stack.head.asInstanceOf[Operator].code.equals("if")){

                  if (stack.head.isInstanceOf[Operator]) {
                    outPut = outPut.enqueue(stack.head) //push to queue
                  }
                  stack = stack.tail //pop
                  if (stack.tail.isEmpty) {
                  stackIsEmpty = true}
                }
                //only if remains so push it to queue
                outPut = outPut.enqueue(stack.head) //push to queue

                stack = stack.tail//pop from stack
              }else{
                //normal else -> push to queue until "if" token

                while (!stackIsEmpty && !stack.head.asInstanceOf[Operator].code.equals("if")){
                  //push to queue
                  outPut = outPut.enqueue(stack.head)
                  //pop
                  stack = stack.tail
                  if(stack.isEmpty){stackIsEmpty = true} //stop is stack is empty
                }//stop if "if" has been reached
              }
              //add else to stack
              stack = com :: stack

            }
          /*returning of final output*/
          if (!tokenList.tail.isEmpty) {
            return outPut ++ convertToReversePolish(tokenList.tail) //skip comments
          } else {
            return outPut
          }
      }
    }
  }


   /*
  * The problems of if-then-else case:
  * First problem: The three parts ("if", "then", "else") are depending on each other. Much similar to brackets
  * they are separating the expression into logical spaces. In my research I figured out, that it would be best to summarize all these parts into one operator called "ifCondition". This operator needs three operands
  * (the condition-, the then- and the else-case). "If", "then" and "else" are all read as tokens, but "then" and "else"
  * are only comments, so they will never appear in the outputQueue or in  reverse polish.
  * But "else" and "then" are still needed for the operator stack. "Else" will even be pushed onto the stack, thus it
  * signalizes the end of the if-clause.
  * Rules how they should affect the stack:
  * +"then" gives the right border of the if-condition, so when it appears all the content of the condition should be
  * pushed to the output queue and popped from the operator stack.
  * +"else" stands for the right corner of the "then" answer and the beginning of the "else" result,
  * so it should put everything from the top of the stack to the next "if" to the queue. If another "else" is already
  * on top of the stack, it means a nested condition. In this case the other if-then-else span has to be put to the queue first.
  * It is important, that "else" is saved on the stack instead of the other "else" and that "then" is never saved on the stack.
  * */


  var resultStack = List[Token]()

  //3.translate reverse polish and calculate the result
  def translateRPToResultStack(outputQueue: Queue[Token]): List[Token] = {
    //it contains on last token, that is a number an contains the answer

    outputQueue.head match {
      case num: Number => // push it to resultStack
        resultStack = num :: resultStack //push
         //pop from queue

        if (!outputQueue.tail.isEmpty) {
          //continue on remaining operators
          return translateRPToResultStack(outputQueue.tail) ::: resultStack
        }
        return resultStack


      case opt: Operator => //pop needed amount of operators from resultStack, perform operation an push result back on resultStack

        if (opt.numberOfOperands == 2) {
          //pop two operands from stack
          val lhs = resultStack.head.asInstanceOf[Number]
          resultStack = resultStack.tail //pop
          val rhs = resultStack.head.asInstanceOf[Number]
          resultStack = resultStack.tail //pop

          val resultToken = Number(eval(lhs, rhs, opt).toString)

          resultStack = resultToken :: resultStack //push result token back on the stack

          if (!outputQueue.tail.isEmpty) {
            return translateRPToResultStack(outputQueue.tail) ::: resultStack //continue translating
          } else {

            return resultStack
          }
        }
        if(opt.numberOfOperands == 3){
          //pop three operands from stack

          val resultElse = resultStack.head.asInstanceOf[Number]
          resultStack = resultStack.tail //pop
          val resultThen = resultStack.head.asInstanceOf[Number]
          resultStack = resultStack.tail //pop
          val condition = resultStack.head.asInstanceOf[Number]
          resultStack = resultStack.tail //pop


          val resultToken = Number(eval3(condition, resultThen, resultElse, opt).toString)
          resultStack = resultToken :: resultStack //push result token back on the stack

          if (!outputQueue.tail.isEmpty) {
            return translateRPToResultStack(outputQueue.tail) ::: resultStack //continue translating
          } else {

            return resultStack
          }
        }

        return translateRPToResultStack(outputQueue.tail) ::: resultStack //if operator did not fit

      case bracket: Bracket =>
        if (!outputQueue.tail.isEmpty) {
          return translateRPToResultStack(outputQueue.tail)
        }
        return List()

      case com: Comment =>
        if (!outputQueue.tail.isEmpty) {
          return translateRPToResultStack(outputQueue.tail)
        } else {
          return List()
        }
    }
  }

  def eval(lhs: Number, rhs: Number, operator: Operator): Int = {

    if (operator.code equals "+") {
      return lhs.code.toInt + rhs.code.toInt
    }
    if (operator.code equals "-") {

      return rhs.code.toInt - lhs.code.toInt //invert to save left to right importance
    }
    if (operator.code equals "*") {
      return lhs.code.toInt * rhs.code.toInt
    }
    if (operator.code equals "/") {
      return rhs.code.toInt / lhs.code.toInt //invert to save left to right importance
    }
    if (operator.code equals "^") {

      return math.pow(rhs.code.toInt , lhs.code.toInt).toInt //invert to save left to right importance
    }
    if (operator.code equals "=="){
      if(lhs.code equals rhs.code){
        return 1 //true
      }else{
        return 0 //false
      }
    }

    return 0
  }

  def eval3(lhs:Number, mid:Number, rhs:Number, operator: Operator): Int ={
    if(operator.code equals "if"){
        //lhs is the condition
        //mid is then
        //rhs is else
      if(lhs.code equals "1"){ //"1" stands for boolean value true
        return mid.code.toInt
      }else{
        return rhs.code.toInt
      }

    }
    return 0
  }

  def extractResultFromResultStack(rStack: List[Token]): Int = { //simple give back the remaining result from stack
    val num = rStack.head

    return num.asInstanceOf[Number].code.toInt
  }

  /*Abstract syntax tree*/

  /*To get abstract syntax trees from shunting yard, I thought it would be best to take the resulting reverse
  * polish of the algorithm and build a abstract syntax tree from it. That way we do not need any fancy grammar to define.
  * This will work similar to translating reverse polish except that now parts of the tree like leafs and
  * branches will be pushed to the resultStack.
  * The last tree element in the resultStack will be the root of the tree, containing everything needed.
  * */

  /*First we need the classic tree elements.
  * To make this documentation more readable I will initialize them here and not at the top of the ShuntingYard Object.
  * */

  sealed trait Tree
  case class Branch(symbol: Symbol,children:List[Tree]) extends Tree//for logical operators
  case class Leaf(symbol: Symbol,code:String) extends Tree //For numbers

  def parseAEWithSYandTrees(code:String): Int ={

    val step1 = createTreeWithSY(code)
    val step2 = evalAST(step1)
    return step2
  }

  def createTreeWithSY(code:String):Tree = {
    treeStack = List() //clear old stack
    val step1 = tokenizeExpression2(code)
    val step2 = convertToReversePolish(step1)
    val step3 = growASTfromReversePolish(step2)

    println("Your Tree: \n" + step3.head.treeString)
    return step3.head
  }

  //I again need a stack for this so  here is a treeStack
  var treeStack = List[Tree]()

  //List[Tree] is the result stack with the tree in it
  def growASTfromReversePolish(outputQueue: Queue[Token]):List[Tree]= {
    outputQueue.head match{
      case com:Comment=>
        if(!outputQueue.tail.isEmpty){
          growASTfromReversePolish(outputQueue.tail) //skip comment
        }else{
          List.empty //do not create anything from a comment.
        }

      //if it is a number create a Leaf from it
      case num:Number =>
        treeStack = Leaf('num,num.code) :: treeStack //push the leaf to stack
        if (!outputQueue.tail.isEmpty){
          return  growASTfromReversePolish(outputQueue.tail) ::: treeStack
        }else{
          return treeStack
        }

       // it can not be an bracket anymore but we need to treat this for the compiler to get rid of some warnings
      case b:Bracket=>
        if(!outputQueue.tail.isEmpty){
          growASTfromReversePolish(outputQueue.tail) //skip bracket
        }else{
          List.empty //do not create anything from a bracket.
        }


        //if it is an operator (a little bit more tricky)

      case opt:Operator=>
        //We want now to:
        //1. pop all needed operands from stack (they are leafs or branches)
        //2.combine them as a branch under the operator
        //3.push that branch back to the stack

        //popping
        if(opt.numberOfOperands == 2){
          //pop two operands from stack
          val lhs = treeStack.head
          treeStack = treeStack.tail //pop
          val rhs = treeStack.head
          treeStack = treeStack.tail //pop

          treeStack = Branch(Symbol(opt.code), List(lhs,rhs)) :: treeStack //push back on the stack

           if (!outputQueue.tail.isEmpty) {
             return growASTfromReversePolish(outputQueue.tail) ::: treeStack
           }else{
             return treeStack
           }
        }

        if(opt.numberOfOperands == 3){
          //pop two operands from stack
          val resultElse = treeStack.head
          treeStack = treeStack.tail //pop
          val resultThen = treeStack.head
          treeStack = treeStack.tail //pop
          val condition = treeStack.head
          treeStack = treeStack.tail //pop

          treeStack = Branch(Symbol(opt.code), List(condition,resultThen,resultElse)) :: treeStack //push back on the stack

          if (!outputQueue.tail.isEmpty) {
            return growASTfromReversePolish(outputQueue.tail) ::: treeStack
          }else{
            return treeStack
          }
        }


        return treeStack.tail //no known operator

    }
  }
    /*Now that we have created a AST tree, we want to evaluate it and get the result*/

    def evalAST(ast:Tree):Int ={
      ast match {
        case branch:Branch =>
          //no we need to know all the possible operations. This is a very weak point, because for every operator
          //we add, we have to rewrite this code
          branch.children.count(p => p.isInstanceOf[Tree]) match {
            case 2 =>
              branch.symbol match {
                case '+ => evalAST(branch.children(0)) +  evalAST(branch.children(1))
                case '- => evalAST(branch.children(1)) -  evalAST(branch.children(0))
                case '/ => evalAST(branch.children(1)) /  evalAST(branch.children(0))
                case '* => evalAST(branch.children(1)) *  evalAST(branch.children(0))
                case '^ => math.pow( evalAST(branch.children(1)),  evalAST(branch.children(0))).toInt
                case '== =>
                  if( evalAST(branch.children(1)) ==  evalAST(branch.children(0))){
                  return 1
                }else{
                  return 0
                }
              }

            case 3=>
              if(evalAST(branch.children(0)) == 1){
                evalAST(branch.children(1))
              }else{
                evalAST(branch.children(2))
              }
          }


        case leaf:Leaf =>
          //return number
          leaf.code.toInt
      }
    }


}
