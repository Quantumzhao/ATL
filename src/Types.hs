module Types
    (
        Variable, 
        Value(..), 
        Expr(..),
        Pattern(..), 
        Statement(..), 
        Program,
        TypeExpr(..),
        Unchecked
    ) 
    where

import Control.Monad.Except (Except)

type Variable = String
data Value = Int Int
           | Bool Bool
           | Array [Value]
           | LinkedList [Value]
           | Struct [(Variable, Value)]
           | Function [Variable] [Statement]
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
          | StructExpr [(Variable, Expr)]
          | Variable Variable
data Pattern = Match Value
             | Bind Variable
data Statement = Assign Variable Expr
               | AssignDefine Variable Expr
               | If Expr [Statement] [Statement]
               | While Expr [Statement]
               | Switch Expr [Pattern]
               | Return Expr
               | Break
data TypeExpr = TypeVar Variable
              | Product Variable Variable
              | Union Variable Variable
              | Dependent Expr
              | TypeArray
              | TypeLList
type Program = [Statement]
type Unchecked = Except String
instance Eq Value where
    Int i1 == Int i2 = i1 == i2
    Bool b1 == Bool b2 = b1 == b2
    Array a1 == Array a2 = a1 == a2
    LinkedList l1 == LinkedList l2 = l1 == l2
    Struct s1 == Struct s2 = s1 == s2
    Function _ _ == Function _ _ = False
    _ == _ = False
