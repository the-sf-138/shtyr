module RLanguage where

data RIdentifier = RIdentified String deriving (Show)

instance Eq RIdentifier where
  RIdentified a == RIdentified b = a == b


data RStrangeName = RStrange String deriving (Show)

instance Eq RStrangeName where
  RStrange a == RStrange b = a == b

  
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
  RInteger x == RInteger y = x == y
  RNumeric x == RNumeric y = x == y
  RString x == RString y = x == y
  _ == _ = False


data RFunctionReference = RFunctionIdentifier RIdentifier
    | RFunctionStrangeName RStrangeName
    | RFunctionReferenceExpression RExpression deriving (Show)

instance Eq RFunctionReference where
  RFunctionStrangeName a == RFunctionStrangeName b = a == b
  RFunctionIdentifier a == RFunctionIdentifier b = a == b
  RFunctionReferenceExpression e == RFunctionReferenceExpression f = e == f
  _ == _ = False


data RFunctionArgument = RTaggedFunctionArgument RFunctionArgumentTag RExpression
  | RSimpleFunctionArgument RExpression
  | REllipses
  | REllipsesN Int deriving (Show)

instance Eq RFunctionArgument where
  RTaggedFunctionArgument a e == RTaggedFunctionArgument b f = (a == b) && (e == f)
  RSimpleFunctionArgument e == RSimpleFunctionArgument f = e == f
  REllipses == REllipses = True
  REllipsesN x == REllipsesN y = x == y
  _ == _ = False


data RFunctionArgumentTag = RTagIdentifier RIdentifier
  | RStrangeTag RStrangeName deriving (Show)

instance Eq RFunctionArgumentTag where
  RTagIdentifier t == RTagIdentifier s = s == t
  RStrangeTag t == RStrangeTag s = s == t
  _ == _ = False


data RExpression = FunctionCall RFunctionReference [RFunctionArgument]
  | RCompoundExpression [RExpression]
  | RAssignment RExpression RExpression
  | RConstantExpression RConstant
  | RVariableExpression RIdentifier
  | RStrangeVariableExpression RStrangeName
  | RIdentifierExpression RIdentifier deriving (Show)

instance Eq RExpression where
  FunctionCall a xs == FunctionCall b ys = (a == b) && xs == ys
  RIdentifierExpression a == RIdentifierExpression b = a == b
  RCompoundExpression as == RCompoundExpression bs = as == bs
  RVariableExpression a == RVariableExpression b = a == b
  RConstantExpression a == RConstantExpression b = a == b
  RAssignment l r == RAssignment x y  = (l == x) && (r == y)
  _ == _ = False



