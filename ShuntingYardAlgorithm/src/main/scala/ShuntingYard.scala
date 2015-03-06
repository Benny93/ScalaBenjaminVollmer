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
  * If the tokens on the STACK ran out while searching for the left side bracket, there is a bracket mismatch. like in an
  * example "5+6)" or "(5+6) + 5)" where the right bracket has no left bracket to match.
  *
  *On the other hand. If the algorithm finds left brackets whilst the final operator popping. We have unmatched left "brackets" like in
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


  def zeroOrMore[Token](parser: => Parser[Token]): Parser[List[Token]] = {
    input => parser(input) match {
      // parse failed; return empty list
      case None =>
        Some((List.empty, input))
      case Some((firstResult, afterFirstResult)) => //if you can parse one try to parse others
        zeroOrMore(parser)(afterFirstResult) match {
          case Some((otherResults, afterOtherResults)) =>
            Some((firstResult :: otherResults, afterOtherResults)) //concatenating of the resulting lists
          case None =>
            None
        }

    }
  }

  //main function
  def parseAEWithShuntingYard(code: String): Int = {

    stack = List() //free variables, because they are public and therefore contain old content
    resultStack = List()

    val step1 = tokenizeExpression(code)
    try {
    val step2 = convertToReversePolish(step1)
    val step3 = translateRPToResultStack(step2)
    return extractResultFromResultStack(step3)
    }catch{
      case e:Exception => println("Exceprtion: " + e + " not a valid Expression!!")
    }
    return 0
  }

  //1. get all the tokens
  def tokenizeExpression(code: String): List[Token] = {
    number(code) match {
      case Some((first, rest)) => List(first) ::: tokenizeExpression(rest)
      case None =>
        plus(code) match {
          case Some((first, rest)) => List(first) ::: tokenizeExpression(rest)
          case None =>
            minus(code) match {
              case Some((first, rest)) => List(first) ::: tokenizeExpression(rest)
              case None =>
                mul(code) match {
                  case Some((first, rest)) => List(first) ::: tokenizeExpression(rest)
                  case None =>
                    div(code) match {
                      case Some((first, rest)) => List(first) ::: tokenizeExpression(rest)
                      case None =>
                        pow(code) match{
                          case Some((first, rest)) => List(first) ::: tokenizeExpression(rest)
                          case None=>
                            leftBracket(code) match {
                              case Some((first, rest)) => List(first) ::: tokenizeExpression(rest)
                              case None =>
                                rightBracket(code) match {
                                  case Some((first, rest)) => List(first) ::: tokenizeExpression(rest)
                                  case None =>
                                    ifCondition(code) match {
                                      case Some((first, last)) => List(first) ::: tokenizeExpression(last)
                                      case None =>
                                        equals(code) match{
                                          case Some((first, last)) => List(first) ::: tokenizeExpression(last)
                                          case None =>
                                            thenKeyword(code) match {
                                              case Some((first, rest)) => List(first) ::: tokenizeExpression(rest)
                                              case None =>
                                                elseKeyword(code) match {
                                                  case Some((first, rest)) => List(first) ::: tokenizeExpression(rest)
                                                  case None =>
                                                    zeroOrMore(whitespace)(code) match {
                                                      case Some((firstResult, afterFResult)) => {

                                                        if (!afterFResult.equals(code)) {

                                                          tokenizeExpression(afterFResult)
                                                        } else {
                                                          List.empty
                                                        }

                                                      }
                                                      case None => List.empty
                                                    }
                                                }
                                            }
                                        }

                                    }

                                }
                            }
                        }

                    }
                }
            }
        }

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
  * The Problematic of if-then-else:
  * First problem: these three parts ("if", "then", "else") are depending on each other. Much similar to brackets
  * they are separating the expression into logical spaces. In my research i figured out, that it would be the best
  * if i summarize all these parts to one operator called "ifCondition". This operator needs three operands
  * (the condition, the then and the else case). "if", "then" and "else" are all read as tokens, but "then" and "else"
  * are only comments, so they will never appear in the outputQueue or reverse polish.
  * But "else" and "then" are still needed for the operator stack. "else" even will be pushed on the stack, thus it
  * signalizes the end of the if clause.
  * Rules how they should effect the stack:
  * +"then" give the right border of the if condition, so when it appears all the content of the condition should be
  * pushed to the output queue and popped from the operator stack.
  * +"else" stands for the right corner of the "then" answer and the beginning of the "else" result,
  * so it should put everything form top of the stack to the next "if" to the queue. if another "else" sits already
  * on top of the stack, it means that it is an nested condition. In this case the other if-then-else span has to be put to the queue first.
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

}
