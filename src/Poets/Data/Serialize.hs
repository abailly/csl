{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances,
  TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Data.Serialize
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Morten Ib Nielsen, Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines (un)marshalling of POETS data.
--
--------------------------------------------------------------------------------
module Poets.Data.Serialize 
    (
     Serialize(..),
     Element,
     Marshal(..),
     serializeToString,
     marshalTerm,
     createAttr,
     empty,
     declQNames
    ) where

import Text.XML.Light
import Data.Comp
import Data.Comp.Derive
import Language.Haskell.TH

-- |Each serializable type must implement this interface.
class Serialize a where
    serialize :: a -> Element
    deSerialize :: Element -> Either String a

serializeToString :: Serialize a => a -> String
serializeToString = showElement . serialize

-- |The marshal algebra. An instance, 'Marshal' @f@, means that terms over the
-- signature @f@ can be serialized to XML.
class Marshal f where
    marshalAlg :: Alg f Element

$(derive [liftSum] [''Marshal])

-- |Serialize a term.
marshalTerm :: (Functor f, Marshal f) => Term f -> Element
marshalTerm = cata marshalAlg

-- |Create an XML attribute.
createAttr :: QName -> String -> Attr
createAttr = Attr

-- |Create an XML element with no namespace.
empty = QName { qName = "", qURI = Nothing, qPrefix = Nothing }

mkQName :: String -> QName
mkQName name = empty { qName = name }

declQName :: String -> Q Dec
declQName name = do
  rhs <- [|mkQName name|]
  return $ FunD (mkName name) [Clause [] (NormalB rhs)  []]

declQNames :: [String] -> Q [Dec]
declQNames = mapM declQName