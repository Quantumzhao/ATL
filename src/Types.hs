module Types
    (
        Variable, 
        Value(..), 
        Expr(..),
        Pattern(..), 
        Statement(..), 
        Program,
        TypeExpr(..),
        Unchecked,
        Signal(..),
        CaseClause,
        Patterns
    ) 
    where

import Control.Monad.Except (Except)

type Variable = String
data Value = Int Int
           | Bool Bool
           | Unit
           | Array [Value]
           | LinkedList [Value]
           | Struct [(Variable, Value)]
           | Function [Variable] [Statement]
           | Exception String {- !!WARNING!! ONLY FOR ERROR -}
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
          | Call Variable [Expr]
data Pattern = Match Value
             | Bind Variable
               {- assign a value without promoting the variable to the subtype
                  e.g. int a =[ a = 5; ]=> int a -}
data Statement = Assign Variable Expr
               {- assign a value and promote the variable to the subtype
                  e.g. int a =[ a = 5; ]=> {5} a -}
               {- Thus,
                  ⊤ a =[ a := 5; ]=> {5} a // O.K. 
                  ⊤ a =[ a = 5; ]=> ⊤ a // valid, but useless -}
               | AssignDefine Variable Expr
               | If Expr [Statement] [Statement]
               | While Expr [Statement]
               | Switch Expr [CaseClause]
               | ReturnX Expr
               | Return
               | Break
               | Impure Expr
data TypeExpr = TypeVar Variable
              | Product Variable Variable
              | Union Variable Variable
              | Dependent Expr
              | TypeArray
              | TypeLList
              | Top
              | Bottom
              | TypeUnit
data Signal = SigReturn
            | SigReturnX Value
            | SigContinue
            | SigBreak
type Program = [Statement]
type Unchecked = Except String
type CaseClause = (Patterns, [Statement])
type Patterns = [Pattern]
instance Eq Value where
    Int i1 == Int i2 = i1 == i2
    Bool b1 == Bool b2 = b1 == b2
    Array a1 == Array a2 = a1 == a2
    LinkedList l1 == LinkedList l2 = l1 == l2
    Struct s1 == Struct s2 = s1 == s2
    _ == _ = False
