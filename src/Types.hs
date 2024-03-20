{-# LANGUAGE GADTs #-}

module Types
  ( Variable
  , Value(..) 
  , Expr(..)
  , VarExpr(..)
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
import Data.List ( intersperse )

data Value = Int Int
           | Bool Bool
           | Unit
           | Array [Value]
           | Struct [(Variable, Value)]
  deriving (Show, Eq)
data VarExpr = VSymbol Variable
             | VIndex VarExpr Expr
             | VProj VarExpr Variable
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
          | XArray [Expr]
          -- | XSpan 
          | XStruct [(Variable, Expr)]
          | XSymbol Variable
          | XCall Variable [Expr]
          | XProj Expr Variable
          | XIndex Expr Expr
data ValuePattern = MatchV Expr
                  | BindV Variable
data ArrayPattern = EmptyA
                  | SkipSomeA ArrayPattern
                  | CheckA Pattern ArrayPattern
data Pattern = PValue ValuePattern
             | PArray ArrayPattern
             | PStruct [RecordEntryPattern]
             | PWildcard
data Statement = SAssign VarExpr Expr
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
              | TTop
              | TBottom
              | TUnit
              | TMap [ID] ID
data TypeInt = IInteger
             | IRange Int Int
             | IUnion TypeInt TypeInt -- a shorthand for TUnion when dealing with integers
             | INumber Int
data TypeBool = BBool
              | BValue Bool
data Signal t = SigReturn t
              | SigContinue
              | SigBreak
  deriving (Show)
type Variable = String
type Procedure = (Variable, [Variable], [Statement])
type Program = [Statement]
type Unchecked = Except String
type CaseClause = (Pattern, [Statement])
type RecordEntryPattern = (Variable, Pattern)
type Index = Int
type ID = Int
