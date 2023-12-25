module Types
    (
        Var, 
        Value(..), 
        Expr(..),
        Case, 
        Statement(..), 
        Program,
        Error
    ) 
    where

type Var = String
data Value = Int Int
           | Bool Bool
           | Array [Value]
           | LinkedList [Value]
           | Struct [(Var, Value)]
           | Function [Var] [Statement]
data Expr = Equal Expr Expr
          | GreaterThan Expr Expr
          | LessThan Expr Expr
          | And Expr Expr
          | Or Expr Expr
          | Not Expr
          | Add Expr Expr
          | Subtract Expr Expr
          | Literal Value
          | ArrayExpr [Expr]
          | LinkedListExpr [Expr]
          | StructExpr [(Var, Expr)]
data Case = C
data Statement = Assign Var Expr
               | If Expr [Statement] [Statement]
               | While Expr [Statement]
               | Switch Expr [Case]
               | Return Expr
type Program = [Statement]
type Error = String

instance Eq Value where
    Int i1 == Int i2 = i1 == i2
    Bool b1 == Bool b2 = b1 == b2
    Array a1 == Array a2 = a1 == a2
    LinkedList l1 == LinkedList l2 = l1 == l2
    Struct s1 == Struct s2 = s1 == s2
    Function _ _ == Function _ _ = False
    _ == _ = False
