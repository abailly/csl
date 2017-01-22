{-# LANGUAGE TypeOperators, FlexibleInstances,TypeSynonymInstances,
  TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Data.Render
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved, Patrick Bahr, and Morten Ib Nielsen
-- Stability   :  unknown
-- Portability :  unknown
--
-- The render algebra (pretty printing).
--
--------------------------------------------------------------------------------
module Poets.Data.Render
    (
     Pretty(..),
     Render(..),
     renderTerm,
     showF
    ) where

import Text.PrettyPrint.Leijen
import Data.Comp
import Data.Comp.Derive hiding (showF)

-- |The render algebra. An instance, 'Render' @f@, means that terms over the
-- signature @f@ can be rendered as 'Doc's.
class Render f where
    render :: Alg f Doc

$(derive [liftSum] [''Render])

instance (Functor f, Render f, Functor g, Render g) => Pretty (f (Term g)) where
    pretty = render . fmap renderTerm

instance (Functor f, Render f) => Show (Term f) where
    show = show . renderTerm

-- |Pretty printing of a term.
renderTerm :: (Functor f, Render f) => Term f -> Doc
renderTerm = cata render

showF :: (Show a, Functor f, Render f) => f a -> String
showF = show . render . fmap (text . show)