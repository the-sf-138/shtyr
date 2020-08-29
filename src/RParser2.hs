module RParser2 where

import Text.ParserCombinators.Parsec as Parsec 
import Data.Char


data RIdentifier = RIdentified String deriving (Show)

data RStrangeName = RStrange String deriving (Show)

data RFunctionReference = RFunctionIdentifier RIdentifier
    | RFunctionStrangeName RStrangeName
    | RFunctionReferenceExpression RExpression deriving (Show)

data RFunctionArgument = RTaggedFunctionArgument RFunctionTag RExpression
  | RSimpleFunctionArgument RExpression
  | REllipses
  | REllipsesN Int deriving (Show)

data RFunctionTag = RTagIdentifier RIdentifier
  | RStrangeTag RStrangeName deriving (Show)

data RExpression = FunctionCall RFunctionReference [RFunctionArgument] deriving (Show)

data RConstant = RNull
  | RInf
  | RNa
  | RNaN
  | RTrue
  | RFalse
  | RNumeric Float
  | RInteger Int
  | RString String deriving (Show)


-- constants
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
rString = (try rSingleQuotedString) <|> rDoubleQuotedString

rSingleQuotedString :: GenParser Char st RConstant
rSingleQuotedString = do 
   char '\''
   s <- (manyTill rStringCharacter (try $ char '\"'))
   return $ RString s

rDoubleQuotedString :: GenParser Char st RConstant
rDoubleQuotedString = do 
   char '\"'
   s <- (manyTill rStringCharacter (try $ char '\"'))
   return $ RString s

rConstant :: GenParser Char st RConstant
rConstant =
  (try rNULL) <|>
  (try rInf) <|>
  (try rNA) <|>
  (try rNaN) <|>
  (try rTRUE) <|>
  (try rFALSE) <|>
  (try rNumeric) <|>
  (try rInteger) <|>
  (try rString)

-- TODO
rStringCharacter :: GenParser Char st Char
rStringCharacter = alphaNum


-- Annoying stuff for the escaped bois
escapedChars :: GenParser Char st Char
escapedChars = oneOf "'\"nrtbafv\\" -- some other dumb shit im not handling

escapedSeq :: GenParser Char st String
escapedSeq = do
  char '\\'
  e <- escapedChars
  return $ concat ["\\", [e]]

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

-- special infix operators are any printable characters delimited by %. the escape sequences for strings do not apply
functionCall :: GenParser Char st RExpression
functionCall = do
    fref <- functionReference
    char '('
    char ')'
    return $ FunctionCall fref []

functionReference :: GenParser Char st RFunctionReference
functionReference = do
  return $ RFunctionIdentifier $ RIdentified "Hello"


parseR :: String -> Either ParseError RExpression
parseR input = parse functionCall "(unknown)" input

parseRConstant :: String -> Either ParseError RConstant
parseRConstant input = parse rConstant "(unknown)" input
