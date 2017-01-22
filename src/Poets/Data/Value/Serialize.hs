{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TypeSynonymInstances,
  FlexibleInstances, TemplateHaskell, TypeOperators #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Data.Value.Serialize
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Morten Ib Nielsen, Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines instances of 'Poets.Data.Serialize.Serialize' for
-- 'Value' and 'VRecord' 'Value', i.e., XML serialization of values
-- and records.
--
--------------------------------------------------------------------------------
module Poets.Data.Value.Serialize
    (
    )
    where

import Control.Monad.Error
import Data.Either
import Poets.Data.Serialize
import Data.Comp
import Poets.Data.Value
import Poets.Data.Value.Utils
import Text.XML.Light
import Text.ParserCombinators.Parsec hiding (string)
import Data.Char

--------------------------------------------------------------------------------
-- Serialization of values
--------------------------------------------------------------------------------

-- DEFINITION OF ELEMENT AND ATTRIBUTE NAMES
$(declQNames ["bool", "real", "field", "int", "list", "name", "record",
              "entity", "entId", "string", "date", "time", "datetime",
              "duration", "value", "year", "month", "day", "hour", "minute",
              "second", "microsecond", "years", "months", "weeks", "days",
              "hours", "minutes", "seconds"])

-- |Marshaling of POETS integers. Output format:
-- <http://www.w3.org/TR/xmlschema-2/#integer>
int2xml :: Int -> String
int2xml = show

-- |Unmarshaling of POETS integers. Input format:
-- <http://www.w3.org/TR/xmlschema-2/#integer>
xml2int :: String -> Either String Int
xml2int = either (throwError . show) return . parse parseInt "Integer parser"
    where parseInt :: GenParser Char st Int
          parseInt = do
            sign <- option [] $ (char '-' >> return "-") <|>
                                (char '+' >> return [])
            ds <- many1 digit
            return $ read $ sign ++ ds

-- |Marshaling of POETS booleans. Output format:
-- <http://www.w3.org/TR/xmlschema-2/#boolean>
bool2xml :: Bool -> String
bool2xml(False) = "false"
bool2xml(True) = "true"

-- |Unmarshaling of POETS booleans. Input format:
-- <http://www.w3.org/TR/xmlschema-2/#boolean>
xml2bool :: String -> Either String Bool
xml2bool str = xml2bool' $ map toLower str
    where
      xml2bool'("false") = return False
      xml2bool'("true") = return True
      xml2bool' s = throwError $ "Failed to parse '" ++ s ++ "' as a boolean"

-- |Marshaling of POETS strings. Output format:
-- <http://www.w3.org/TR/xmlschema-2/#string>
string2xml :: String -> String
string2xml = id

-- |Unmarshaling of POETS string. Input format:
-- <http://www.w3.org/TR/xmlschema-2/#string>
xml2string :: String -> Either String String
xml2string = return

-- |Marshaling of doubles. Output format:
-- <http://www.w3.org/TR/xmlschema-2/#double>
double2xml :: Double -> String
double2xml = show

-- |Unmarshaling of doubles. Input format:
-- <http://www.w3.org/TR/xmlschema-2/#double>
-- TODO: Does not yet accept all input strings (such as \"10e2\").
xml2double :: String -> Either String Double
xml2double = either (throwError . show) return . parse parseDouble "Double parser"
    where parseDouble :: GenParser Char st Double
          parseDouble = do
            sign <- option [] $ (char '-' >> return "-") <|>
                                (char '+' >> return [])
            ds <- many1 digit
            fracs <- option [] $ do
                          char '.'
                          ds <- many1 digit
                          return $ '.' : ds
            return $ read $ sign ++ ds ++ fracs


instance Marshal VRecord where
    marshalAlg (VR rn rf) =
        let fields = map marshalAlg (fieldsList rf)
            n = node record fields
            v = createAttr name rn
        in add_attr v n

instance Marshal VField where
    marshalAlg (VF fn fv) =
        let n = node field fv
            v = createAttr name fn
        in add_attr v n

instance Marshal Val where
    marshalAlg (VInt i) =
        let n = node int () 
            v = createAttr value $ int2xml i
        in add_attr v n
    marshalAlg (VBool b) =
        let n = node bool () 
            v = createAttr value $ bool2xml b
        in add_attr v n
    marshalAlg (VString s) =
        let n = node string () 
            v = createAttr value $ string2xml s
        in add_attr v n
    marshalAlg (VDate d) = 
        let n = node date ()
            (y, m, da) = dateToYearMonthDay d
            crAtt n v = createAttr n $ int2xml v
            ay   = crAtt year (fromIntegral y)
            amon = crAtt month m
            aday = crAtt day da
        in add_attrs [ay,amon,aday] n
    marshalAlg (VTime t) = 
        let n = node time ()
            (h, mi, s, ms) = timeToHourMinSecMicroSec t
            crAtt n v = createAttr n $ int2xml v
            ah   = crAtt hour h
            amin = crAtt minute mi
            as   = crAtt second s
            ams  = crAtt microsecond ms
        in add_attrs [ah,amin,as,ams] n
    marshalAlg (VDateTime dt) = 
        let n = node datetime ()
            (d, t) = dateTimeToDateTime dt
            (y, m, da) = dateToYearMonthDay d
            (h, mi, s, ms) = timeToHourMinSecMicroSec t
            crAtt n v = createAttr n $ int2xml v
            ay   = crAtt year (fromIntegral y)
            amon = crAtt month m
            aday = crAtt day da
            ah   = crAtt hour h
            amin = crAtt minute mi
            as   = crAtt second s
            ams  = crAtt microsecond ms
        in add_attrs [ay,amon,aday,ah,amin,as,ams] n
    marshalAlg (VDuration (VD s min h d w mon y)) = 
        let nd = node duration ()
            crAtt n v = createAttr n $ int2xml v
            as   = crAtt seconds s
            amin = crAtt minutes min
            ah   = crAtt hours h
            ad   = crAtt days d
            aw   = crAtt weeks w
            amon = crAtt months mon
            ay   = crAtt years y
        in add_attrs [as,amin,ah,ad,aw,amon,ay] nd
    marshalAlg (VReal d) = 
        let n = node real () 
            v = createAttr value $ double2xml d
        in add_attr v n
    marshalAlg (VList l) =
        node list l
    marshalAlg (VEnt VEntity{ventType = entName, ventId = id}) =
        let nr = node entity ()
            aName = createAttr name entName
            aId = createAttr entId $ int2xml id
        in add_attrs [aName, aId] nr
    marshalAlg (VRecord vr) =
        marshalAlg vr


--------------------------------------------------------------------------------
-- Unserialization
--------------------------------------------------------------------------------

instance Serialize Value where
    serialize = value2xml
    deSerialize = xml2value

instance Serialize (VRecord Value) where
    serialize = value2xml . iVRecord
    deSerialize = xml2vrecord

-- Add the name space for values
addNamespace :: Element -> Element
addNamespace = let xmlns = empty { qName = "xmlns" }
                   v = createAttr xmlns "http://poets.diku.dk/values"
               in add_attr v

--DEFINITION OF (UN)MARSHALLING SERVICES
value2xml :: Value -> Element
value2xml = addNamespace . marshalTerm

xml2vrecord  :: (Val :<: e) => Element -> Either String (VRecord (Term e))
xml2vrecord = processRecord'

--ELEMENT TO VALUE TRANSLATION
checkElementName :: QName -> QName -> Either String ()
checkElementName exp found =
    when (qName exp /= qName found)
         (throwError $ "Expected element '" ++ qName exp ++
                       "' but found '" ++ qName found ++ "'")

findAttr' :: QName -> Element -> Either String String
findAttr' a e =
    maybe (throwError $ "Attribute '" ++ qName a ++ "' not found")
          return
          (findAttr a e)


xml2value :: (Val :<: e) => Element -> Either String (Term e)
xml2value e =
    let process = [processBool, processInt, processReal, processTime,
                   processDate, processDateTime, processDuration, processString,
                   processList, processRecord, processEnt]
        v = rights $ map (\f -> f e) process
    in
      if null v then
          throwError "Failed to parse value"
      else
          return $ head v

processT :: (g :<: f) =>
            QName ->
            (b -> g (Term f)) ->
            (String -> Either String b) -> 
            Element -> 
            Either String (Term f)
processT qname valConstr transform e = do
  checkElementName qname (elName e)
  v <- findAttr' value e
  v' <- transform v
  return $ (inject . valConstr) v'

processBool :: (Val :<: e) => Element -> Either String (Term e)
processBool = processT bool VBool xml2bool

processInt :: (Val :<: e) => Element -> Either String (Term e)
processInt = processT int VInt xml2int

processReal :: (Val :<: e) => Element -> Either String (Term e)
processReal = processT real VReal xml2double

processDate :: (Val :<: e) => Element -> Either String (Term e)
processDate e = do
  checkElementName date (elName e)
  y <- read year
  m <- read month
  d <- read day
  date <- maybe (throwError "Failed to parse date") return
                (createDate (fromIntegral y) m d)
  return $ inject $ VDate date
    where read :: QName -> Either String Int
          read name = findAttr' name e >>= xml2int

processTime :: (Val :<: e) => Element -> Either String (Term e)
processTime e = do
  checkElementName time (elName e)
  h <- read hour
  mi <- read minute
  s <- read second
  ms <- read microsecond
  time <- maybe (throwError "Failed to parse time") return
                (createTime h mi s ms)
  return $ inject $ VTime time
    where read :: QName -> Either String Int
          read name = findAttr' name e >>= xml2int

processDateTime :: (Val :<: e) => Element -> Either String (Term e)
processDateTime e = do
  checkElementName datetime (elName e)
  y <- read year
  m <- read month
  d <- read day
  h <- read hour
  mi <- read minute
  s <- read second
  ms <- read microsecond
  let date = createDate (fromIntegral y) m d
  let time = createTime h mi s ms
  datetime <- maybe (throwError "Failed to parse datetime") return
                    (liftM2 createDateTime date time)
  return $ inject $ VDateTime datetime
    where read :: QName -> Either String Int
          read name = findAttr' name e >>= xml2int

processDuration :: (Val :<: e) => Element -> Either String (Term e)
processDuration e = do
  checkElementName duration (elName e)
  s <- read seconds
  min <- read minutes
  h <- read hours
  d <- read days
  w <- read weeks
  mon <- read months
  y <- read years
  return $ inject $ VDuration (VD s min h d w mon y)
    where read :: QName -> Either String Int
          read name = findAttr' name e >>= xml2int

processString :: (Val :<: e) => Element -> Either String (Term e)
processString = processT string VString xml2string

processList :: (Val :<: e) => Element -> Either String (Term e)
processList e = do
  checkElementName list (elName e)
  let body = filterChildren (\_ -> True) e
  vs <- mapM xml2value body
  return $ (inject . VList) vs

processRecord :: (Val :<: e) => Element -> Either String (Term e)
processRecord e = fmap (inject . VRecord) $ processRecord' e

processRecord' :: (Val :<: e) => Element -> Either String (VRecord (Term e))
processRecord' e = do
  checkElementName record (elName e)
  rn <- findAttr' name e
  let fields = filterChildren (\_ -> True) e
  fs <- mapM processField fields
  return $ VR rn (newFields fs)

processField :: (Val :<: e) => Element -> Either String (VField (Term e))
processField e = do
  checkElementName field (elName e)
  fn <- findAttr' name e
  vElem <- maybe (throwError $ "No child found for tag '" ++ qName field ++ "'")
                 return
                 (filterChild (\_ -> True) e)
  v <- xml2value vElem
  return $ VF fn v

processEnt :: (Val :<: e) => Element -> Either String (Term e)
processEnt e = do
  checkElementName entity (elName e)
  entId' <- findAttr' entId e
  entId <- xml2int entId'
  entName <- findAttr' name e
  return $ iVEnt VEntity{ventType = entName,ventId = entId,ventContext = Nothing}