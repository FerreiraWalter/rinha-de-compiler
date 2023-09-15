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

	} deriving(Show, Generic)

data Second = Second
	{

	} deriving(Show, Generic)


data Term = Term
 {

 } deriving(Show, Generic)















