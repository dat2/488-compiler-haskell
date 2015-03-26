module AST where

data LangType = IntegerType | BooleanType | None
  deriving (Show, Eq)

stringToDecl :: String -> LangType
stringToDecl "integer" = IntegerType
stringToDecl "boolean" = BooleanType
stringToDecl _ = None

data Decl =
    ScalarDecl { declType :: LangType, declIdent :: String }
  | ArrayDecl { declType :: LangType, declIdent :: String, lowerBound1 :: Integer, upperBound1 :: Integer, lowerBound2 :: Integer, upperBound2 :: Integer }
  deriving Show

data RoutineDecl =
    FunctionDecl { name :: String, params :: [Decl], routineBody :: [Stmt], functionType :: LangType }
  | ProcedureDecl { name :: String, params :: [Decl], routineBody :: [Stmt] }
  deriving Show

data Stmt =
    Scope { body :: [Stmt] }
  | AssignStmt { left :: Expn, right :: Expn }
  | IfThenStmt { condition :: Expn, trueBody :: [Stmt], falseBody :: [Stmt] }
  | WhileDoStmt { condition :: Expn, body :: [Stmt] }
  | LoopStmt { body :: [Stmt] }
  | ExitStmt { stmtExpn :: Expn }
  | ReturnStmt { stmtExpn :: Expn }
  | PutStmt { puts :: [Expn] }
  | GetStmt { gets :: [Expn] }
  | DeclStmt { decls :: [Decl] }
  deriving Show

data ArithOperator = Plus | Minus | Multiply | Divide deriving Show
data BoolOperator = And | Or deriving Show
data UnaryOperator = UnaryNot | UnaryMinus deriving Show
data CompareOperator = Eq | Neq | Lt | Leq | Gt | Geq deriving Show

data Expn =
    IdentExpn { identifier :: String }
  | SubsExpn { identifier :: String }
  | UnaryExpn { uop :: UnaryOperator, expn :: Expn }
  | ArithExpn { aop :: ArithOperator, lhs :: Expn, rhs :: Expn }
  | BoolExpn { bop :: BoolOperator, lhs :: Expn, rhs :: Expn }
  | CompareExpn { op :: CompareOperator, lhs :: Expn, rhs :: Expn }
  | AnonFuncExpn { funcBody :: [Stmt], expn :: Expn }
  | FuncCallExpn { identifier :: String, args :: [Expn] }
  | TextConstExpn { text :: String }
  | IntConstExpn { int :: Integer }
  | BoolConstExpn { bool :: Bool }
  | NullExpn
  deriving Show
