module RParser where

import Text.ParserCombinators.Parsec as Parsec 
import Data.Char

import RLanguage


-------------------------------------
-- constants
-------------------------------------
rNULL :: GenParser Char st RConstant
rNULL = do
  string "NULL"
  return RNull

rInf :: GenParser Char st RConstant
rInf = do
  string "Inf"
  return RInf

rNA :: GenParser Char st RConstant
rNA = do
  string "NA"
  return RNa

rNaN :: GenParser Char st RConstant
rNaN = do
  string "NaN"
  return RNaN

rTRUE :: GenParser Char st RConstant
rTRUE  = do
  string "TRUE"
  return RTrue

rFALSE :: GenParser Char st RConstant
rFALSE = do
  string "FALSE"
  return RFalse

rNumeric :: GenParser Char st RConstant
rNumeric = do
  string "<NUMERIC>"
  return $ RNumeric 1.0

rInteger :: GenParser Char st RConstant
rInteger = do
  string "<INTEGER>"
  return $ RInteger 1


rString :: GenParser Char st RConstant
rString = try rSingleQuotedString <|> rDoubleQuotedString


rIdentifier :: GenParser Char st RIdentifier
rIdentifier = do
  s <- many1 rIdentifierChar
  return $ RIdentified s


rIdentifierChar :: GenParser Char st Char
rIdentifierChar = alphaNum <|> oneOf "_."



rSingleQuotedString :: GenParser Char st RConstant
rSingleQuotedString = do 
   char '\''
   s <- many (rStringCharacter '\'')
   char '\''
   return $ RString s

rDoubleQuotedString :: GenParser Char st RConstant
rDoubleQuotedString = do 
   char '\"'
   s <- many (rStringCharacter '"')
   char '\"'
   return $ RString s


rConstant :: GenParser Char st RConstant
rConstant =
  try rNULL <|>
  try rInf <|>
  try rNA <|>
  try rNaN <|>
  try rTRUE <|>
  try rFALSE <|>
  try rNumeric <|>
  try rInteger <|>
  try rString

-- If we see the escape char we have to check if we are escaping the end of the quoted sequence
-- else we can just return the character
rStringCharacter :: Char -> GenParser Char st Char
rStringCharacter c = noneOf ("\\" ++  [c])
                     <|> try (string ("\\" ++ [c]) >> return c)
                     <|> noneOf [c]


-- Identifiers are a sequence of letters, digits & '.' and the underscore. They must not start with a digit or an underscore
-- or with a period  followed by a digit

-- ellipse
ellipse :: GenParser Char st RFunctionArgument
ellipse = do
  string "..."
  return REllipses

ellipseN :: GenParser Char st RFunctionArgument
ellipseN = do
  string ".."
  n <- digit
  return $ REllipsesN (read [n]::Int)

-- RExpressions
rWhitespace :: GenParser Char st RExpression
rWhitespace = oneOf "\n" >> return RWhitespace

rEndOfFile :: GenParser Char st RExpression
rEndOfFile = eof >> return REndOfFile
  
  
-- RExpressions
rVariableExpression :: GenParser Char st RExpression
rVariableExpression = RVariableExpression <$> rIdentifier

rConstantExpression :: GenParser Char st RExpression
rConstantExpression = RConstantExpression <$> rConstant


-- TODO This should probably also handle EOF
rEndOfExpression :: GenParser Char st Char
rEndOfExpression = oneOf "\n;"


rTerminatedBy e p = do
  v <- p
  e
  return v

rExpressionTerminatedBy e =
  try rEndOfFile
  <|> try rWhitespace
  <|> try (expressionTerminator rVariableExpression)
  <|> try (expressionTerminator assignmentExpression)
  <|> try (expressionTerminator functionCall)
  where expressionTerminator = rTerminatedBy e

rNonEmptyExpressionTerminatedBy e =
  try rWhitespace
  <|> try (expressionTerminator rVariableExpression)
  <|> try (expressionTerminator assignmentExpression)
  <|> try (expressionTerminator functionCall)
  where expressionTerminator = rTerminatedBy e

rFullExpression :: GenParser Char st RExpression
rFullExpression = rExpressionTerminatedBy (oneOf ";\n")

rNonEmptyExpression :: GenParser Char st RExpression
rNonEmptyExpression  = rNonEmptyExpressionTerminatedBy (oneOf ";\n")

rFunctionReference :: GenParser Char st RExpression
rFunctionReference = rExpressionTerminatedBy (oneOf "(")

rFunctionArgumentExpression :: GenParser Char st RExpression
rFunctionArgumentExpression = rExpressionTerminatedBy (oneOf ",)")


-- special infix operators are any printable characters delimited by %. the escape sequences for strings do not apply
functionCall :: GenParser Char st RExpression
functionCall = do
    fref <- rFunctionReference
    char ')'
    return $ FunctionCall fref []


emptyArgumentList :: GenParser Char st [RFunctionArgument]
emptyArgumentList = do
  char ')'
  return []

functionReferenceStrangeName :: GenParser Char st RFunctionReference
functionReferenceStrangeName = rStringToStrangeName <$> rString

rStringToStrangeName :: RConstant -> RFunctionReference
rStringToStrangeName (RString s) = RFunctionStrangeName (RStrange s)

functionReferenceIdentifier :: GenParser Char st RFunctionReference
functionReferenceIdentifier = RFunctionIdentifier <$> rIdentifier

rFunctionArgumentList :: GenParser Char st [RFunctionArgument]
rFunctionArgumentList = do
  r <- sepBy singleFunctionArgument rFunctionArgumentSeparation
  char ')'
  return r

rFunctionArgumentSeparation :: GenParser Char st String
rFunctionArgumentSeparation = do
  spaces
  char ','
  spaces
  return ""

singleFunctionArgument :: GenParser Char st RFunctionArgument
singleFunctionArgument = try taggedFunctionArgument <|> simpleFunctionArgument

simpleFunctionArgument :: GenParser Char st RFunctionArgument
simpleFunctionArgument = try simpleFunctionArgumentExpression <|> simpleFunctionArgumentIdentifier

simpleFunctionArgumentIdentifier :: GenParser Char st RFunctionArgument
simpleFunctionArgumentIdentifier = RSimpleFunctionArgument . RVariableExpression <$> rIdentifier

simpleFunctionArgumentExpression :: GenParser Char st RFunctionArgument
simpleFunctionArgumentExpression = RSimpleFunctionArgument <$> rFunctionArgumentExpression

taggedFunctionArgument :: GenParser Char st RFunctionArgument
taggedFunctionArgument = do
  tag <- try functionArgumentTagIdentifier <|> functionArgumentTagStrange
  char '='
  e <- rFunctionArgumentExpression
  return $ RTaggedFunctionArgument tag e

assignmentExpression :: GenParser Char st RExpression
assignmentExpression = do
  lhs <- rVariableExpression
  spaces
  char '='
  spaces
  rhs <- rConstantExpression
  return $ RAssignment lhs rhs


functionArgumentTagIdentifier :: GenParser Char st RFunctionArgumentTag
functionArgumentTagIdentifier = RTagIdentifier <$> rIdentifier

functionArgumentTagStrange :: GenParser Char st RFunctionArgumentTag
functionArgumentTagStrange = undefined 

-- Compound Expression
compoundExpression :: GenParser Char st RExpression
compoundExpression = do
  char '{'
  es <- compoundExpressionInner
  char '}'
  return $ RCompoundExpression es

compoundExpressionInner :: GenParser Char st [RExpression]
compoundExpressionInner = many rFullExpression

-- For handling whole files and such
rFile :: GenParser Char st [RExpression]
rFile = manyTill rNonEmptyExpression eof


parseR :: String -> Either ParseError [RExpression]
parseR = parse rFile "(unknown)" 

parseRExpression :: String -> Either ParseError RExpression
parseRExpression = parse rFullExpression "(unknown)" 


parseFunctionArgument :: String -> Either ParseError RFunctionArgument
parseFunctionArgument = parse singleFunctionArgument "(unknown)"

parseRConstant :: String -> Either ParseError RConstant
parseRConstant = parse rConstant "(unknown)"


