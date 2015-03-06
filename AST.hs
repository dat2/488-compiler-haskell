module AST where

data LangType = IntegerType | BooleanType | None
  deriving Show

stringToDecl :: String -> LangType
stringToDecl "integer" = IntegerType
stringToDecl "boolean" = BooleanType
stringToDecl _ = None

data Decl =
    ScalarDecl { declType :: LangType, declIdent :: String }
  deriving Show

data Stmt =
    Scope { body :: [Stmt] }
  | AssignStmt { lhs :: Expn, rhs :: Expn }
  | IfThenStmt { condition :: Expn, trueBody :: [Stmt], falseBody :: [Stmt] }
  | WhileDoStmt { condition :: Expn, body :: [Stmt] }
  | LoopStmt { body :: [Stmt] }
  | ExitStmt { expn :: Expn }
  | ReturnStmt { expn :: Expn }
  | DeclStmt { decls :: [Decl] }
  deriving Show

data Expn =
    IdentExpn { identifier :: String }
  | TextConstExpn { text :: String }
  | IntConstExpn { int :: Integer }
  | BoolConstExpn { bool :: Bool }
  | NullExpn
  deriving Show
