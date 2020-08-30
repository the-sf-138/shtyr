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

data RExpression = FunctionCall RFunctionReference [RFunctionArgument]
  | RIdentifierExpression RIdentifier deriving (Show)

data RConstant = RNull
  | RInf
  | RNa
  | RNaN
  | RTrue
  | RFalse
  | RNumeric Float
  | RInteger Int
  | RString String deriving (Show)

instance Eq RConstant where
  RInf == RInf = True
  RNull == RNull = True
  RNaN == RNaN = True
  RTrue == RTrue = True
  RFalse == RFalse = True
  RInteger x == RInteger y = (x == y)
  RNumeric x == RNumeric y = (x == y)
  RString x == RString y = (x == y)
  _ == _ = False

instance Eq RExpression where
  FunctionCall a xs == FunctionCall b ys = (a == b) && (xs == ys)
  RIdentifierExpression a == RIdentifierExpression b = (a == b)
  _ == _ = False

instance Eq RFunctionReference where
  RFunctionStrangeName a == RFunctionStrangeName b = (a == b)
  RFunctionIdentifier a == RFunctionIdentifier b = (a == b)
  RFunctionReferenceExpression e == RFunctionReferenceExpression f = (e == f)
  _ == _ = False

instance Eq RStrangeName where
  RStrange a == RStrange b = (a == b)

instance Eq RIdentifier where
  RIdentified a == RIdentified b = (a == b)

instance Eq RFunctionArgument where
  RTaggedFunctionArgument a e == RTaggedFunctionArgument b f = (a == b) && (e == f)
  RSimpleFunctionArgument e == RSimpleFunctionArgument f = (e == f)
  REllipses == REllipses = True
  REllipsesN x == REllipsesN y = (x == y)
  _ == _ = False

instance Eq RFunctionTag where
  RTagIdentifier t == RTagIdentifier s = (s == t)
  RStrangeTag t == RStrangeTag s = (s == t)
  _ == _ = False

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

rIdentifier :: GenParser Char st RIdentifier
rIdentifier = do
  s <- many1 rIdentifierChar
  return $ RIdentified s

rIdentifierChar :: GenParser Char st Char
rIdentifierChar = alphaNum <|> (oneOf "_.")

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
  (try rNULL) <|>
  (try rInf) <|>
  (try rNA) <|>
  (try rNaN) <|>
  (try rTRUE) <|>
  (try rFALSE) <|>
  (try rNumeric) <|>
  (try rInteger) <|>
  (try rString)

-- If we see the escape char we have to check if we are escaping the end of the quoted sequence
-- else we can just return the character
rStringCharacter :: Char -> GenParser Char st Char
rStringCharacter c = noneOf (concat ["\\", [c]])
                     <|> try (string (concat ["\\", [c]]) >> return c)
                     <|> noneOf [c]

-- Annoying stuff for the escaped bois
-- TODO not used yet
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


expression :: GenParser Char st RExpression
expression = functionCall

-- special infix operators are any printable characters delimited by %. the escape sequences for strings do not apply
functionCall :: GenParser Char st RExpression
functionCall = do
    fref <- functionReference
    char '('
    args <- emptyArgumentList <|> rFunctionArgumentList
    return $ FunctionCall fref args

functionReference :: GenParser Char st RFunctionReference
functionReference = (try functionReferenceIdentifier)
  <|> (try functionReferenceStrangeName)
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
functionReferenceStrangeName = do
  s <- rString
  return $ rStringToStrangeName s

rStringToStrangeName :: RConstant -> RFunctionReference
rStringToStrangeName (RString s) = RFunctionStrangeName (RStrange s)

functionReferenceIdentifier :: GenParser Char st RFunctionReference
functionReferenceIdentifier = do
  s <- rIdentifier
  return $ RFunctionIdentifier s

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
singleFunctionArgument = simpleFunctionArgument <|> taggedFunctionArgument

simpleFunctionArgument :: GenParser Char st RFunctionArgument
simpleFunctionArgument = simpleFunctionArgumentIdentifier <|> simpleFunctionArgumentExpression

simpleFunctionArgumentIdentifier :: GenParser Char st RFunctionArgument
simpleFunctionArgumentIdentifier = do
  i <- rIdentifier
  return $ RSimpleFunctionArgument $ RIdentifierExpression i

simpleFunctionArgumentExpression :: GenParser Char st RFunctionArgument
simpleFunctionArgumentExpression = do
  e <- expression
  return $ RSimpleFunctionArgument $ e

taggedFunctionArgument :: GenParser Char st RFunctionArgument
taggedFunctionArgument = undefined

parseR :: String -> Either ParseError RExpression
parseR input = parse functionCall "(unknown)" input

parseRConstant :: String -> Either ParseError RConstant
parseRConstant input = parse rConstant "(unknown)" input


