import GHC.Generics (Generic)

data File = File 
	{
		fileName :: String,
		fileExpression :: Term,
		fileLocation :: Location
	} deriving(Show, Generic)

data Location = Location
	{
		locationStart :: Integer,
		locationEnd :: Integer,
		locationFilename :: String
	} deriving(Show, Generic)

data Parameter = Parameter
	{
		parameterText :: String,
		parameterLocation :: Location
	} deriving(Show, Generic)

data Var = Var 
	{
		varKind :: String,
		varText :: String
	} deriving(Show, Generic)

data Function = Function
	{
		functionKind :: String,
		functionParameters :: Parameter,
		functionValue :: Term,
		functionLocation :: Location
	} deriving(Show, Generic)

data Call = Call 
	{
		callKind :: String,
		callCallee :: Term,
		callArguments :: [Term],
		callLocation :: Location
	} deriving(Show, Generic)

data Let = Let
 {
	letKind :: String,
	letName :: Parameter,
	letValue :: Term,
	letNext :: Term,
	letLocation :: Location
 } deriving(Show, Generic)

data Str = Str 
	{
		strKind :: String,
		strValue :: String,
		strLocation :: Location
	} deriving(Show, Generic)

data Int = Int
	{
		intKing :: String,
		intValue :: Integer,
		intLocation :: Location
	} deriving(Show, Generic)

data Boolean = Bool 
	{
		booleanKind :: String,
		booleanValue :: Bool,
		booleanLocation :: Location
	} deriving(Show, Generic)

data If = If 
	{
		ifKind :: String,
		ifCondition :: Term,
		ifThen :: Term,
		ifOtherwise :: Term,
		ifLocation :: Location
	} deriving(Show, Generic)

data Binary = Binary 
	{
		binaryKind :: String,
		binaryLhs :: Term,
		binaryOp :: BinaryOp,
		binaryRhs :: Term,
		binaryLocation :: Location
	} deriving(Show, Generic)

data Tuple = Tuple 
	{
		tupleKind :: String,
		tupleFirst :: Term,
		tupleSecond :: Term,
		tupleLocation :: Location
	} deriving(Show, Generic)

data First = First
	{
		firstKind :: String,
		firstValue :: Term,
		firstLocarion :: Location
	} deriving(Show, Generic)

data Second = Second
	{

	} deriving(Show, Generic)


data BinaryOp
  = Concat -- Concatenate, +
  | Add -- Add, +
  | Sub -- Subtract, -
  | Mul -- Multiply, *
  | Div -- Divide, /
  | Rem -- Rem, %
  | Eq -- Equal, ==
  | Neq -- Not equal, !=
  | Lt -- Less than, <
  | Gt -- Greater than, >
  | Lte -- Less than or equal to, <=
  | Gte -- Greater than or equal to, >=
  | And -- And, &&
  | Or -- Or, ||
  deriving (Show)

data Term
  = BoolTerm
      { termKind :: String
      , boolValue :: Bool
      , termLocation :: Location
      }
  | IntTerm
      { termKind :: String
      , integerValue :: Integer
      , termLocation :: Location
      }
  | StringTerm
      { termKind :: String
      , stringValue :: String
      , termLocation :: Location
      }
  | VarTerm
      { termKind :: String
      , text :: String
      , termLocation :: Location
      }
  | FunctionTerm
      { termKind :: String
      , parameters :: [Var]
      , value :: Term
      , termLocation :: Location
      }
  | CallTerm
      { termKind :: String
      , callee :: Term
      , arguments :: [Term]
      , termLocation :: Location
      }
  | BinaryTerm
      { termKind :: String
      , lhs :: Term
      , op :: BinaryOp
      , rhs :: Term
      , termLocation :: Location
      }
  | LetTerm
      { termKind :: String
      , name :: Var
      , value :: Term
      , next :: Term
      , termLocation :: Location
      }
  | IfTerm
      { termKind :: String
      , condition :: Term
      , thenBranch :: Term
      , otherwiseBranch :: Term
      , termLocation :: Location
      }
  | PrintTerm
      { termKind :: String
      , printValue :: Term
      , termLocation :: Location
      }
  deriving (Show, Generic)
