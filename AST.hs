module AST where

data AST =
  Scope { body :: [AST] }
  | AssignStmt { lhs :: String, rhs :: AST }
  | IfThenStmt { condition :: AST, body :: [AST] }
  | IdentExpr { identifier :: String } deriving Show
