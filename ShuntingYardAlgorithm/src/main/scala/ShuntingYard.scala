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
* |   |   while(there is an operator j ON TOP of STACK with greater persistence than t)
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
  case class Bracket(code:String,leftsided :Boolean) extends Token
  case class Operator(code:String,numberOfOperands:Byte,levelOfPersistence: Byte) extends Token

  //Parsing basics for tokenize
  def numberParser(): Parser[Token] =
    parseRegex("[0-9]+") ^^ { x => Number(x) }

  def operatorParser(operator: String,numberOfOperands:Byte,levelOfPersistence: Byte): Parser[Token] =
    parseString(operator) ^^ { x => Operator(x,numberOfOperands,levelOfPersistence) }

  def bracketParser(bracket:String):Parser[Token]=
    parseString(bracket)^^{x => Bracket(x,x == "(")}


  val number = numberParser()
  val plus = operatorParser("+",2,0)
  val minus = operatorParser("-",2,1)
  val mul = operatorParser("*",2,2)
  val div = operatorParser("/",2,2)
  val leftBracket = bracketParser("(")
  val rightBracket = bracketParser(")")



  //main function
  def parseAEWithShuntingYard(code:String):Int={???}

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
                          case None=>List.empty
                        }
                      }
                    }
                }
            }
        }

    }

  }



  //2.convert to reverse polish
  def convertToReversePolish(tokensList:List[Token]):Queue[Token]={???}


  //3.translate reverse polish and calculate the result
  def translateRPAndEvalResult(outputQueue:Queue[Token]):Int={???}

}
