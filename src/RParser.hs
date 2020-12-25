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
rVariableExpression :: GenParser Char st RExpression
rVariableExpression = RVariableExpression <$> rIdentifier

rConstantExpression :: GenParser Char st RExpression
rConstantExpression = RConstantExpression <$> rConstant



expression :: GenParser Char st RExpression
expression = try functionCall  <|> try compoundExpression <|> try assignmentExpression <|> try rVariableExpression

-- Need to do this while we are not at the end of the expression
-- XXX this needs to be redone
outterExpression :: GenParser Char st RExpression
outterExpression = do
  e   <- expression
  end <- endOfExpression
  if end
    then
    do
      return e
    else
    do
      char '('
      args <- emptyArgumentList <|> rFunctionArgumentList
      end <- endOfExpression
      return $ FunctionCall (RFunctionReferenceExpression e) args

endOfExpression :: GenParser Char st Bool
endOfExpression = (oneOf ";\n" >> return True) <|> return False


-- special infix operators are any printable characters delimited by %. the escape sequences for strings do not apply
functionCall :: GenParser Char st RExpression
functionCall = do
    fref <- functionReference
    char '('
    args <- emptyArgumentList <|> rFunctionArgumentList
    return $ FunctionCall fref args

functionReference :: GenParser Char st RFunctionReference
functionReference = try functionReferenceIdentifier
  <|> try functionReferenceStrangeName
  <|> functionReferenceExpression


emptyArgumentList :: GenParser Char st [RFunctionArgument]
emptyArgumentList = do
  char ')'
  return []

functionReferenceExpression :: GenParser Char st RFunctionReference
functionReferenceExpression = do
  char '('
  e <- expression 
  char ')'
  return $ RFunctionReferenceExpression e

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
simpleFunctionArgumentExpression = RSimpleFunctionArgument <$> expression

taggedFunctionArgument :: GenParser Char st RFunctionArgument
taggedFunctionArgument = do
  tag <- try functionArgumentTagIdentifier <|> functionArgumentTagStrange
  char '='
  e <- expression
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

compoundExpression :: GenParser Char st RExpression
compoundExpression = do
  char '{'
  es <- compoundExpressionInner
  char '}'
  return $ RCompoundExpression es

compoundExpressionInner :: GenParser Char st [RExpression]
compoundExpressionInner = many outterExpression

parseR :: String -> Either ParseError RExpression
parseR = parse outterExpression "(unknown)" 

parseFunctionArgument :: String -> Either ParseError RFunctionArgument
parseFunctionArgument = parse singleFunctionArgument "(unknown)"

parseRConstant :: String -> Either ParseError RConstant
parseRConstant = parse rConstant "(unknown)"


