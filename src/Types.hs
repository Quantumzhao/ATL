{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Types
  ( Variable
  , Value(..) 
  , Expr(..)
  , Pattern(..) 
  , Statement(..) 
  , Program
  , Procedure
  , TypeExpr(..)
  , Unchecked
  , Signal(..)
  , CaseClause
  , ArrayPattern(..)
  , ValuePattern(..)
  -- , LListPattern(..)
  , RecordEntryPattern
  , TypeInt(..)
  , TypeBool(..) ) 
  where

import Control.Monad.Except ( Except )

data Value = Int Int
           | Bool Bool
           | Null
           | Array [Value]
        --    | LinkedList [Value]
           | Struct [(Variable, Value)]
          --  | Proc [Variable] [Statement]
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
          | XArray [Expr]
          | XStruct [(Variable, Expr)]
          | XVar Variable
          | XCall Variable [Expr]
          | XProj Expr Variable
          | XIndex Expr Expr
data ValuePattern = MatchV Expr
                  | BindV Variable
data ArrayPattern = EmptyA
                  | SkipSomeA ArrayPattern
                  | CheckA Pattern ArrayPattern
-- data LListPattern = EmptyL
--                   | MatchL Expr LListPattern
--                   | BindL Pattern LListPattern
data Pattern = PValue ValuePattern
             | PArray ArrayPattern
            --  | LListP LListPattern
             | PStruct [RecordEntryPattern]
             | PWildcard
data Statement = SAssign Variable Expr
               | SIf Expr [Statement] [Statement]
               | SWhile Expr [Statement]
               | SSwitch Expr [CaseClause]
               | SReturn Expr
               | SBreak
               | SImpure Expr
               | SProcedure Procedure
data TypeExpr = TUnion ID ID
              | TInt TypeInt
              | TBool TypeBool
              | TArray Int ID [(Index, ID)]
              | TStruct [(Variable, ID)]
              -- | TVar Variable
              | TTop
              | TBottom
              | TNull
              | TMap [ID] ID
data TypeInt = IInteger
             | IRange Int Int
             | IUnion TypeInt TypeInt -- a shorthand for TUnion when dealing with integers
             | INumber Int
data TypeBool = BBool
              | BValue Bool
data Signal = SigReturn Value
            | SigContinue
            | SigBreak
type Variable = String
type Procedure = (Variable, [Variable], [Statement])
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
