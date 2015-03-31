module AST where

data LangType = IntegerType | BooleanType | None
  deriving (Show, Eq)

langType :: String -> LangType
langType "integer" = IntegerType
langType "boolean" = BooleanType
langType _ = None

newtype Program = Program { programBody :: [Stmt] } deriving Show

data Decl =
    ScalarDecl { declType :: LangType, declIdent :: String }
  | ArrayDecl { declType :: LangType, declIdent :: String, lowerBound1 :: Integer, upperBound1 :: Integer, lowerBound2 :: Integer, upperBound2 :: Integer }
  deriving Show

data Stmt =
    AssignStmt { left :: Expn, right :: Expn }
  | IfThenStmt { condition :: Expn, trueBody :: [Stmt], falseBody :: [Stmt] }
  | WhileDoStmt { condition :: Expn, body :: [Stmt] }
  | LoopStmt { body :: [Stmt] }
  | ExitStmt { stmtExpn :: Expn }
  | ReturnStmt { stmtExpn :: Expn }
  | PutStmt { outputs :: [Expn] }
  | GetStmt { inputs :: [Expn] } -- todo only use identifiers / arrays
  | ProcedureCallStmt { name :: String, stmtArgs :: [Expn] }
  | Scope { body :: [Stmt] }
  | DeclStmt { decls :: [Decl] }
  | FunctionDecl { name :: String, params :: [Decl], routineBody :: [Stmt], functionType :: LangType }
  | ProcedureDecl { name :: String, params :: [Decl], routineBody :: [Stmt] }
  deriving Show

data ArithOperator = Plus | Minus | Multiply | Divide deriving Show
data BoolOperator = And | Or deriving Show
data UnaryOperator = UnaryNot | UnaryMinus deriving Show
data CompareOperator = Eq | Neq | Lt | Leq | Gt | Geq deriving Show

data Expn =
    IdentExpn { identifier :: String }
  | SubsExpn { identifier :: String, expn1 :: Expn, expn2 :: Expn }
  | UnaryExpn { uop :: UnaryOperator, expn :: Expn }
  | ArithExpn { aop :: ArithOperator, lhs :: Expn, rhs :: Expn }
  | BoolExpn { bop :: BoolOperator, lhs :: Expn, rhs :: Expn }
  | CompareExpn { op :: CompareOperator, lhs :: Expn, rhs :: Expn }
  | AnonFuncExpn { funcBody :: [Stmt], expn :: Expn }
  | FuncCallExpn { identifier :: String, expnArgs :: [Expn] }
  | TextConstExpn { text :: String }
  | IntConstExpn { int :: Integer }
  | BoolConstExpn { bool :: Bool }
  | NullExpn
  deriving Show
