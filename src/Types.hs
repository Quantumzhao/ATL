{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Types
  ( Variable
  , Value(..) 
  , Expr(..)
  , Pattern(..) 
  , Statement(..) 
  , Program
  , TypeExpr(..)
  , Unchecked
  , Signal(..)
  , CaseClause
  , ArrayPattern(..)
  , ValuePattern(..)
  , LListPattern(..)
  , RecordEntryPattern
  , TypeInt(..)
  , TypeBool(..) ) 
  where

import Control.Monad.Except ( Except )

type Variable = String
data Value = Int Int
           | Bool Bool
           | Unit
           | Array [Value]
        --    | LinkedList [Value]
           | Struct [(Variable, Value)]
           | Proc [Variable] [Statement]
          --  | Exception String {- !!WARNING!! ONLY FOR ERROR -}
data Expr = XEqual Expr Expr
          | XGt Expr Expr
          | XLt Expr Expr
          | XAnd Expr Expr
          | XOr Expr Expr
          | XNot Expr
          | XAdd Expr Expr
          | XSub Expr Expr
          | XInt Int
          | XBool Bool
          | XUnit
          | XProc [Variable] [Statement]
          | XArray [Expr]
          | XStruct [(Variable, Expr)]
          | XVar Variable
          | XCall Variable [Expr]
-- data Expr a where
--   Equal :: Expr a -> Expr a -> Expr Bool
--   GreaterThan :: Expr Int -> Expr Int -> Expr Bool
--   LessThan :: Expr Int -> Expr Int -> Expr Bool
--   And :: Expr Bool -> Expr Bool -> Expr Bool
--   Or :: Expr Bool -> Expr Bool -> Expr Bool
--   Not :: Expr Bool -> Expr Bool
--   Add :: Expr Int -> Expr Int -> Expr Int
--   Subtract :: Expr Int -> Expr Int -> Expr Int
--   Literal :: Value -> Expr a
--   ExprArr :: [Expr a] -> Expr a
--   ExprVar :: Variable -> Expr a
--   App :: Variable -> [Expr a] -> Expr a
data ValuePattern = MatchV Expr
                  | BindV Variable
data ArrayPattern = EmptyA
                  | SkipSomeA ArrayPattern
                  | CheckA Pattern ArrayPattern
data LListPattern = EmptyL
                  | MatchL Expr LListPattern
                  | BindL Pattern LListPattern
data Pattern = ValueP ValuePattern
             | ArrayP ArrayPattern
            --  | LListP LListPattern
             | RecordP [RecordEntryPattern]
             | Wildcard
data Statement = Assign Variable Expr
               | AssignDefine Variable Expr
               | If Expr [Statement] [Statement]
               | While Expr [Statement]
               | Switch Expr [CaseClause]
               | ReturnX Expr
               | Return
               | Break
               | Impure Expr
data TypeExpr = TUnion TypeExpr TypeExpr
              | TInt TypeInt
              | TBool TypeBool
              | TArray Expr TypeExpr [(Index, ID)]
              | TStruct [(Variable, TypeExpr)]
              | TVar Variable
              | TTop
              | TBottom
              | TUnit
              | TMap [TypeExpr] TypeExpr
data TypeInt = IInteger
             | IRange Expr Expr
             | INumber Int
data TypeBool = BBool
              | BValue Bool
data Signal = SigReturn
            | SigReturnX Value
            | SigContinue
            | SigBreak
type Program = [Statement]
type Unchecked = Except String
type CaseClause = (Pattern, [Statement])
type RecordEntryPattern = (Variable, Pattern)
type Index = Int
type ID = Int
instance Eq Value where
  (==) :: Value -> Value -> Bool
  Int i1 == Int i2 = i1 == i2
  Bool b1 == Bool b2 = b1 == b2
  Array a1 == Array a2 = a1 == a2
  -- LinkedList l1 == LinkedList l2 = l1 == l2
  Struct s1 == Struct s2 = s1 == s2
  _ == _ = False
