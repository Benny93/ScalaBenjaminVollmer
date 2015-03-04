/**
 * Created by Benny on 01.03.15.
 * Final Project
 */
import sext._

import scala.collection.immutable.Queue

object ShuntingYard extends util.Combinators {
/*
* In this final project Djikstras shunting yard algorithm will be implemented.
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

  case class Number(code:String) extends Token
  case class Bracket(code:String,isLeftBracket :Boolean) extends Token
  case class Operator(code:String,numberOfOperands:Byte,levelOfPersistence: Byte) extends Token
  case class Comment(code:String) extends  Token

  //Parsing basics for tokenize
  def numberParser(): Parser[Token] =
    parseRegex("[0-9]+") ^^ { x => Number(x) }

  def operatorParser(operator: String,numberOfOperands:Byte,levelOfPersistence: Byte): Parser[Token] =
    parseString(operator) ^^ { x => Operator(x,numberOfOperands,levelOfPersistence) }

  def bracketParser(bracket:String):Parser[Token]=
    parseString(bracket)^^{x => Bracket(x,x == "(")}

  def whiteSpaceParser(ws:String):Parser[Token]=
    parseString(ws) ^^{x => Comment(x) }

  val number = numberParser()
  val plus = operatorParser("+",2,0)
  val minus = operatorParser("-",2,1)
  val mul = operatorParser("*",2,2)
  val div = operatorParser("/",2,2)
  val leftBracket = bracketParser("(")
  val rightBracket = bracketParser(")")
  val whitespace = whiteSpaceParser(" ")

  def zeroOrMore[Token](parser: => Parser[Token]): Parser[List[Token]] = {
    input => parser(input) match {
      // parse failed; return empty list
      case None =>
        Some((List.empty, input))


      case Some((firstResult, afterFirstResult)) =>
        zeroOrMore(parser)(afterFirstResult) match {
          case Some((otherResults, afterOtherResults)) =>
            Some((firstResult :: otherResults, afterOtherResults))

          case None =>
            None
        }

    }
  }
        //main function
  def parseAEWithShuntingYard(code:String):Int={

    stack = List()
    resultStack = List()

    val step1 = tokenizeExpression(code)
    val step2 = convertToReversePolish(step1)
    val step3 = translateRPToResultStack(step2)
    return extractResultFromResultStack(step3)
  }

  //1. get all the tokens
  def tokenizeExpression(code:String):List[Token]={
    number(code) match{
      case Some((Number(x),rest))=>List(Number(x)) ::: tokenizeExpression(rest)
      case Some(_)=> List.empty
      case None=>
        plus(code)match{
          case Some((Operator(a,b,c),rest)) =>List(Operator(a,b,c))::: tokenizeExpression(rest)
          case Some(_)=>List.empty
          case None=>
            minus(code)match{
              case Some((Operator(a,b,c),rest)) =>List(Operator(a,b,c))::: tokenizeExpression(rest)
              case Some(_)=>List.empty
              case None=>
                mul(code) match{
                  case Some((Operator(a,b,c),rest)) =>List(Operator(a,b,c))::: tokenizeExpression(rest)
                  case Some(_)=>List.empty
                  case None=>
                    div(code)match{
                      case Some((Operator(a,b,c),rest)) =>List(Operator(a,b,c))::: tokenizeExpression(rest)
                      case Some(_)=>List.empty
                      case None=>
                      leftBracket(code) match{
                      case Some((Bracket(a,b),rest)) =>List(Bracket(a,b))::: tokenizeExpression(rest)
                      case Some(_)=>List.empty
                      case None=>
                        rightBracket(code) match{
                          case Some((Bracket(a,b),rest)) =>List(Bracket(a,b))::: tokenizeExpression(rest)
                          case Some(_)=>List.empty
                          case None=>
                            zeroOrMore(whitespace)(code)match{
                              case Some((firstResult, afterFResult))=> {
                                //println("this is after first result: " + afterFResult)
                                if(!afterFResult.equals(code)) {
                                  //println("i tokenized this: " + afterFResult)
                                  tokenizeExpression(afterFResult)
                                }else{List.empty}

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


  //Stack is an inelegant and potentially poorly-performing wrapper
  // around List.
  // Use List instead:
  // stack push x becomes x :: list;
  // stack.pop is list.tail.
  var stack = List[Token]()

  //2.convert to reverse polish
  def convertToReversePolish(tokenList:List[Token]): Queue[Token]={
    if (tokenList.isEmpty){
      //pop everthing from stack to queue

      var queue = Queue[Token]()
      //println("stack state" + stack)
      if(!stack.isEmpty) {
        queue =  queue.enqueue(stack)
        while (!stack.isEmpty){
          stack = stack.tail
        }
      }

      return queue
    }else {

      tokenList.head match {
        case number: Number =>
          return Queue[Token]().enqueue(number) ++ convertToReversePolish(tokenList.tail)


        case cOpt: Operator => //Token is operator
          var outPut = Queue[Token]()
          if (!stack.isEmpty) {
            //if stack is not empty
            var otherToken: Token = stack.head //first other Token on stack
            if (otherToken.isInstanceOf[Operator]) {
              var otherOperator: Operator = otherToken.asInstanceOf[Operator]
              var stackIsEmpty:Boolean = false
              while (!stackIsEmpty && stack.head.isInstanceOf[Operator] && otherOperator.levelOfPersistence >= cOpt.levelOfPersistence) {

                outPut = outPut.enqueue(otherOperator)
                //pop other
                stack = stack.tail
                //get new other Operator
                if(!stack.isEmpty) {
                  if (stack.head.isInstanceOf[Operator]) {
                    otherOperator = stack.head.asInstanceOf[Operator]
                  }
                }else{stackIsEmpty = true}
              }
            }

          }

          //push current token to stack
          stack = cOpt :: stack

          val fusion = outPut ++ convertToReversePolish(tokenList.tail) //concatinate outputQueue with future OutputQueue

          return fusion

        case bracket: Bracket =>
          var outPut = Queue[Token]()
          if (bracket.isLeftBracket) {
            //push to stack
            stack = bracket :: stack
            return convertToReversePolish(tokenList.tail)

          } else {
            //TODO catch if no match bracket
            if (!stack.isEmpty) {
              //if stack is not empty
              var popOperator:Boolean = false
              var otherToken: Token = stack.head //first other Token on stack

              if (otherToken.isInstanceOf[Operator]){
                  popOperator = true
              }else{
                  popOperator = false
              }

              //pop everything inside brackets ( ) to queue
              while (popOperator) {
                //
                outPut = outPut.enqueue(otherToken) //pop from stack to queue
                stack = stack.tail //pop first element

                otherToken = stack.head
                if(otherToken.isInstanceOf[Bracket]){
                  // has to be leftbracket because we never push rightbracket to the stack
                  popOperator = false
                }

              }
              // pop left bracket -> has to be left bracket now beacuse we popped everything before it
              if (otherToken.asInstanceOf[Bracket].isLeftBracket) {
                //pop leftbracket and discard
                stack = stack.tail
              }
            }

            return outPut ++ convertToReversePolish(tokenList.tail)
          }

         case com:Comment=>
          if(!tokenList.tail.isEmpty) {
            return convertToReversePolish(tokenList.tail) //skip comments
          }else{return Queue()}
      }
    }
  }

  var resultStack = List[Token]()
  //3.translate reverse polish and calculate the result
  def translateRPToResultStack(outputQueue:Queue[Token]):List[Token]={ //it contains on last token, that is a number an contains the answer
    println("queue to translate " + outputQueue)
    outputQueue.head match{
      case num:Number=>  // push it to resultStack
          resultStack = num :: resultStack //push
          var remainQ = outputQueue.tail //pop from queue

        if (!remainQ.isEmpty){
          println("remaining Queue after numbers "  + remainQ)
           //continue on remaining operators
          return translateRPToResultStack(remainQ) ::: resultStack
        }
        return resultStack


      case opt:Operator=>  //pop needed amount of operators from resultStack, perform operation an push result back on resultStack
        //println("Operator Info : " + opt.code + opt.numberOfOperands)
        if(opt.numberOfOperands == 2 ){
              //pop two operands from stack
              println("My resultStack " + resultStack)

              val lhs = resultStack.head.asInstanceOf[Number]
              resultStack = resultStack.tail //pop
              val rhs = resultStack.head.asInstanceOf[Number]
              resultStack = resultStack.tail //pop

              val resultToken = Number(eval(lhs,rhs,opt).toString)

              resultStack = resultToken :: resultStack //push result token back on the stack

            if(!outputQueue.tail.isEmpty) {
              return translateRPToResultStack(outputQueue.tail) ::: resultStack //continue translating
            }else{

              return resultStack
            }
          }
          //TODO 3 Operands

        return translateRPToResultStack(outputQueue.tail) ::: resultStack //if operator did not fit

      case bracket:Bracket =>
        if(!outputQueue.tail.isEmpty){
          return translateRPToResultStack(outputQueue.tail)
        }
        return List()

      case com:Comment=>
        if(!outputQueue.tail.isEmpty) {
          return translateRPToResultStack(outputQueue.tail)
        }else{return List()}
    }
  }

  def eval(lhs:Number, rhs:Number, operator: Operator): Int ={
    println("operator " + operator.code )
    if(operator.code equals  "+"){
      return lhs.code.toInt + rhs.code.toInt
    }
    if(operator.code equals  "-"){

      return  rhs.code.toInt - lhs.code.toInt //invert to save left to right importance
    }
    if(operator.code equals  "*"){
      return lhs.code.toInt * rhs.code.toInt
    }
    if(operator.code equals  "/"){
      return  rhs.code.toInt / lhs.code.toInt
    }
    return 0
  }
  def extractResultFromResultStack(rStack:List[Token]): Int ={
      val num = rStack.head

    return  num.asInstanceOf[Number].code.toInt
  }

}
