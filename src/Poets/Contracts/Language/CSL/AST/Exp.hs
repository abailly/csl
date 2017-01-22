{-# LANGUAGE TemplateHaskell, TypeOperators, FlexibleContexts #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Contracts.Language.CSL.AST.Exp
-- Copyright   :  3gERP, 2012
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines the ASTs for CSL expressions and values.
--
--------------------------------------------------------------------------------

module Poets.Contracts.Language.CSL.AST.Exp
    (
     CoreExp(..),
     CaseExp(..),
     Var,
     BinOp(..),
     SugExp(..),
     UnOpSug(..),
     BinOpSug(..),
     VUnit(..),
     VFun(..),
     reportsKeyword,
     dummyVar,
     -- * Smart Constructors
     iEVar,
     iEProj,
     iEUpdate,
     iEBinOp,
     iEApply,
     iELambda,
     iEIfThenElse,
     iECase,
     iCaseExp,
     iESUnOp,
     iESBinOp,
     iESLet,
     iESLambda,
     iESUpdate,
     iVFun,
     iVUnit,
     -- * Smart Constructors w/ Annotations
     iAEVar,
     iAEProj,
     iAEUpdate,
     iAEBinOp,
     iAEApply,
     iAELambda,
     iAEIfThenElse,
     iAECase,
     iACaseExp,
     iAESUnOp,
     iAESBinOp,
     iAESLet,
     iAESLambda,
     iAESUpdate,
     iAVUnit
    ) where

import Prelude hiding (EQ, LT, GT)
import Poets.Data.Value (FieldName, RecordName)
import Data.Ord (comparing)
import Data.Comp.Derive

-- |Signature for (core) expressions in CSL.
data CoreExp e =
    -- |Variable.
    EVar{
      expVar :: Var -- ^Variable name.
    }
    -- |Anonymous function, lambda abstraction.
  | ELambda{
      expLambdaParam :: Var, -- ^Paramater.
      expLambdaBody :: e -- ^Function body.
    }
    -- |Function application.
  | EApply{
      expApplyFn :: e, -- ^Function expression.
      expApplyArg :: e -- ^Argument expression.
    }
    -- |Field projection.
  | EProj{
      expProjFieldName :: FieldName, -- ^Field to project.
      expProjRecord :: e -- ^Record expression.
    }
    -- |Field updating.
  | EUpdate{
      expUpdateRecord :: e, -- ^Record expression.
      expUpdateFieldName :: FieldName, -- ^Field to update.
      expUpdateValue :: e -- ^Update expression.
    }
    -- |Binary operator.
  | EBinOp{
      expBinOp :: BinOp,  -- ^Operator type.
      expBinOpArg1 :: e, -- ^First argument.
      expBinOpArg2 :: e -- ^Second argument.
    }
    -- |Conditional expression.
  | EIfThenElse{
      expCondition :: e, -- ^Conditional expression.
      expConditionThen :: e, -- ^Then branch.
      expConditionElse :: e -- ^Else branch.
    }
    -- |Case expression.
  | ECase{
      expCase :: e, -- ^The expression to dispatch on.
      expCaseType :: Maybe RecordName, -- ^Type annotation for uniqueness.
      expCases :: [CaseExp e] -- ^The different cases.
    }

-- |A branch in a case expression
data CaseExp e = CaseExp{
      -- |Match this case if the value is a sub type of the specified record.
      caseExpRecordName :: RecordName,
      -- |Bind the value to this variable.
      caseExpVar :: Var,
      -- |The body of the branch.
      caseExpBody :: e}

-- |Variables are encoded as strings.
type Var = String

-- |Binary operators in CSL expressions.
data BinOp = EQ -- ^Equals, @==@.
           | LEQ -- ^Less than or equals, @\<=@.
           | PLUS -- ^Plus, @+@.
           | TIMES -- ^Times, @*@.
           | DIV -- ^Division, @/@.
           | AND -- ^Conjunction, @&&@.
           | CONS -- ^List concatenation, @:@.
           | DPLUS -- ^Addition of duration values, @\<+\>@.
           | DTIMES -- ^Scaling of duration values, @\<*\>@.
           deriving (Eq,Ord)

instance Show BinOp where
    show EQ = "=="
    show LEQ = "<="
    show PLUS = "+"
    show TIMES = "*"
    show DIV = "/"
    show AND = "&&"
    show CONS = "#"
    show DPLUS = "<+>"
    show DTIMES = "<*>"

-- |Signature for derived expressions in CSL.
data SugExp e =
    -- |Unary operator
    ESUnOp{
      expSugUnOp :: UnOpSug, -- ^Operator type.
      expSugUnOpArg :: e -- ^Argument.
    }
    -- |Binary operator.
  | ESBinOp{
      expSugBinOp :: BinOpSug, -- ^Operator type.
      expSugBinOpArg1 :: e, -- ^First argument.
      expSugBinOpArg2 :: e -- ^Second argument.
    }
    -- |Let expression.
  | ESLet{
      expSugLetName :: Var, -- ^Local name.
      expSugLetDef :: e, -- ^Definition.
      expSugLetBody :: e -- ^Let body.
    }
    -- |Anonymous function, lambda abstraction, multiple parameters.
  | ESLambda{
      expSugLambdaParams :: [Var], -- ^Paramaters.
      expSugLambdaBody :: e -- ^Function body.
    }
    -- |Multiple field updating.
  | ESUpdate{
      expSugUpdateRecord :: e, -- ^Record expression.
      expSugUpdateList :: [(FieldName, e)]
    }

-- |Derived unary operators in CSL expressions.
data UnOpSug = NOT -- ^Negation, @not@
             | UMINUS -- ^Unary minus, @-@.
               deriving (Eq,Ord)

-- |Derived binary operators in CSL expressions.
data BinOpSug = NEQ -- ^Inequality, @/=@
              | GEQ -- ^Greater than or equals, @\>=@.
              | LT -- ^Less than, @\<@.
              | GT -- ^Greater than, @\>@.
              | MINUS -- ^Minus, @-@.
              | DMINUS -- ^Subtraction of duration values, @\<-\>@.
              | OR -- ^Disjunction, @||@.
                deriving (Eq,Ord)

instance Show UnOpSug where
    show NOT = "not"
    show UMINUS = "-"

instance Show BinOpSug where
    show NEQ = "/="
    show GEQ = ">="
    show LT = "<"
    show GT = ">"
    show MINUS = "-"
    show DMINUS = "<->"
    show OR = "||"

-- |Signature for unit value.
data VUnit e = VUnit

-- |Signature for function values. Since functions are suspended computations,
-- they may produce errors upon application, which is realized via the
-- @Either String@ monad.
data VFun e = VFun (e -> Either String e)

instance EqF VFun where
  _ `eqF` _ = error "cannot test functions for equality"

-- |A special identifier for accessing reports within expressions.
reportsKeyword :: Var
reportsKeyword = "reports"

-- |A special \"dummy\" variable is used in places where the parser allows
-- binding occurrences to be optional, but the AST still requires the binding.
dummyVar :: Var
dummyVar = "_"

instance Eq a => Eq (CaseExp a) where
    (==) ce1 ce2 = caseExpBody ce1 == caseExpBody ce2

instance Ord a => Ord (CaseExp a) where
    compare = comparing caseExpBody

$(derive [makeFunctor, makeFoldable, makeTraversable, makeEqF,
          makeOrdF, smartConstructors, smartAConstructors]
         [''CaseExp, ''CoreExp, ''VUnit])

$(derive [makeFunctor, makeEqF, makeOrdF, smartConstructors, smartAConstructors]
         [''SugExp])

$(derive [smartConstructors] [''VFun])