module RParser where

import Text.ParserCombinators.Parsec as Parsec 
import Data.Char

type Operator = String

data RExpression = RVariable String
    | RIntLiteral Int
    | RStringLiteral String
    | RFloatLiteral String
    | NormalFunctionCall RExpression [RExpression] 
    | InfixFunctionCall RExpression Operator RExpression
    | OperatorFunctionCall RExpression Operator RExpression deriving (Show)


-- All the operators in R (that don't have the surrounding %)
rOperator :: GenParser Char st String
rOperator = string "+"
    <|> string "-"
    <|> string "*"
    <|> string "/"
    <|> string "="
    <|> string "<-"
    <|> string "->"

rFile :: GenParser Char st [RExpression]  
rFile = do
  result <- many expression
  eof
  return result

expression :: GenParser Char st RExpression
expression =  try normalFunctionCall
  <|> try infixFunctionCall
  <|> try operatorFunctionCall
  <|> rVariable 
  <|> rIntLiteral

rVariable = do
  name <- rVariableName
  return $ RVariable name

rVariableName = do
  firstChar <- (upper <|> lower)
  rest <- many (satisfy isAlphaNum)
  return (firstChar:rest)


allowSurroundingWhitespace :: (GenParser Char st RExpression) -> GenParser Char st RExpression
allowSurroundingWhitespace p = do
    spaces;
    r <- p
    spaces;
    return r


normalFunctionCall = do
  functionName <- allowSurroundingWhitespace rVariable
  open <- char '('
  args <- functionArguments
  close <- char ')'
  return $ NormalFunctionCall functionName args

functionArguments = do
  spaces;
  f <- singleFunctionArgument
  r <- remainingFunctionArguments
  return (f:r)

singleFunctionArgument = expression

remainingFunctionArguments =
  (char ',' >>  functionArguments) <|> (return [])

infixFunctionCall = do
  lhs <- allowSurroundingWhitespace expression
  char '%'
  infixName <- rVariableName
  char '%'
  rhs <- allowSurroundingWhitespace expression
  return $ InfixFunctionCall lhs infixName rhs

operatorFunctionCall = do
  lhs <- allowSurroundingWhitespace expression
  operator <- rOperator
  rhs <- allowSurroundingWhitespace expression
  return $ InfixFunctionCall lhs operator rhs

rIntLiteral :: GenParser Char st RExpression
rIntLiteral = do
  s <- many digit
  return $ RIntLiteral (read s::Int)


sampleAssignment = "foo = bar"

parseR :: String -> Either ParseError [RExpression]
parseR input = parse rFile "(unknown)" input
