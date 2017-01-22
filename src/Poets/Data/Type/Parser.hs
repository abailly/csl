--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Data.Type.Parser
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Mikkel Jønsson Thomsen, Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module provides a parser for the POETS dialect of Attempto Controlled 
-- English (ACE).
--
--------------------------------------------------------------------------------
module Poets.Data.Type.Parser
    (
     ParseError,
     parseFile,
     parsePCE
    ) where

import Text.Parsec.String hiding (Parser)
import Text.Parsec.Language
import qualified Text.Parsec.Token as P
import Text.Parsec hiding (Empty,string)
import Data.Char
import qualified Data.Set as Set
import Poets.Data.Type
import Poets.Data.Type.Render
import Poets.Data.Value (RecordName, FieldName)
import Control.Monad (liftM)

-- The generated record environment is stored in the state
type Parser a = GenParser Char POETSRecordEnv a

-- Definition of the general lexer
lexer :: P.TokenParser POETSRecordEnv
lexer = P.makeTokenParser pceDef

pceDef = P.LanguageDef {
          P.commentStart = "",
          P.commentEnd = "",
          P.nestedComments = False,
          P.opStart = P.opStart haskellStyle,
          P.opLetter = P.opLetter haskellStyle,
          P.commentLine = "#",
          P.identStart = lower,
          P.identLetter = alphaNum <|> oneOf "_",
          P.caseSensitive = True,
          P.reservedOpNames = ["."],
          P.reservedNames = ["has","a","an","list","field","in","is",
                             "called","named","comment","abstract", "entity",
                             "entities", "order", "hidden", "restricted", "by",
                             "report"] ++
                            -- Types
                            ["Int","Bool","String","Date","Time","DateTime",
                             "Real","Duration"]
         }

--------------------------------------------------------------------------------
-- Alias definitions for the lexer
--------------------------------------------------------------------------------

whiteSpace = P.whiteSpace lexer
reservedOp = P.reservedOp lexer
reserved = P.reserved lexer
lexeme = P.lexeme lexer
identifier = P.identifier lexer
integer = P.integer lexer

--------------------------------------------------------------------------------
-- Utility functions for accessing/updating the record environment
--------------------------------------------------------------------------------

getRec :: RecordName -> Parser (Record Type)
getRec n = do
  rEnv <- getState
  return $ case getRecordInfo rEnv n of
             Right x -> x
             Left _ -> Record{recordName = n,
                              recordFields = newFieldEnv [],
                              recordExtends = Set.empty,
                              recordAttributes = Set.empty}

setRec :: Record Type -> Parser ()
setRec r = do
  rEnv <- getState
  setState $ addRecordInfo rEnv r

getField :: Record Type -> FieldName -> Field Type
getField rInfo n = case getFieldInfo (recordFields rInfo) n of
                     Right x -> x
                     Left _ -> Field{fieldName = n,
                                     fieldType = iTBool, -- hack
                                     fieldAttributes = Set.empty}

--------------------------------------------------------------------------------
-- PCE definitions
--------------------------------------------------------------------------------

-- |Parse file containing PCE.
parseFile :: FilePath -> IO (POETSRecordEnv -> Either ParseError POETSRecordEnv)
parseFile path = do
  cont <- readFile path
  return $ parsePCE path cont

-- |Parse a string containing PCE.
parsePCE :: SourceName 
         -> String
         -> POETSRecordEnv
         -> Either ParseError POETSRecordEnv
parsePCE path cont rEnv = runParser p rEnv path cont
    where p = do whiteSpace
                 many $ lexeme pceItem
                 eof
                 getState

pceItem :: Parser ()
pceItem = do r <- recordIdentifier
             ((reserved "is" >> isSentence r) <|>
              (reserved "has" >> hasSentence r)) <|>
              (reserved "field" >> fieldAttribute r)
             reservedOp "."

fieldAttribute :: RecordName -> Parser ()
fieldAttribute r = do
  f <- fieldIdentifier
  fieldOrder r f <|> fieldRestriction r f

fieldOrder :: RecordName -> FieldName -> Parser ()
fieldOrder r f = do
  reserved "has"
  reserved "order"
  n <- liftM fromIntegral integer
  rInfo <- getRec r
  let field = addFieldAttribute (getField rInfo f) (FieldOrder n)
  setRec $ rInfo{recordFields = addFieldInfo (recordFields rInfo) field}

fieldRestriction :: RecordName -> FieldName -> Parser ()
fieldRestriction r f = do
  reserved "is"
  reserved "restricted"
  reserved "by"
  reserved "report"
  rep <- reportIdentifier
  rInfo <- getRec r
  let field = addFieldAttribute (getField rInfo f) (FieldRestriction rep)
  setRec $ rInfo{recordFields = addFieldInfo (recordFields rInfo) field}

isSentence :: RecordName -> Parser ()
isSentence n = recordAttribute n <|> superClass n

recordAttribute :: RecordName -> Parser ()
recordAttribute n = abstract n <|> hidden n

abstract :: RecordName -> Parser ()
abstract n = do
  reserved "abstract"
  rInfo <- getRec n
  setRec $ addRecordAttribute rInfo RecordIsAbstract

hidden :: RecordName -> Parser ()
hidden n = do
  reserved "hidden"
  rInfo <- getRec n
  setRec $ addRecordAttribute rInfo RecordIsHidden

superClass :: RecordName -> Parser ()
superClass n = do
  option () quantifierSingle
  c <- recordIdentifier
  rInfo <- getRec n
  setRec $ rInfo{recordExtends = Set.insert c (recordExtends rInfo)}

quantifierSingle :: Parser ()
quantifierSingle = reserved "an" <|> reserved "a" <|> reserved "in"

hasSentence :: RecordName -> Parser ()
hasSentence = fieldDefinition

fieldDefinition :: RecordName -> Parser ()
fieldDefinition n = do
  (fieldType, defaultName) <- fieldType
  name <- fieldName defaultName
  rInfo <- getRec n
  let f = getField rInfo name
  setRec $ rInfo{recordFields = addFieldInfo (recordFields rInfo)
                                             f{fieldType = fieldType}}
  where
    fieldType :: Parser (Type, String)
    fieldType = try (do reserved "a"
                        reserved "list"
                        reserved "of"
                        (tp,name) <- basicType
                        return (iTList tp, name)) <|>
                (option () quantifierSingle >> basicType)
    basicType :: Parser (Type, String)
    basicType = do
      tp <- (reserved "Int" >> return iTInt) <|>
            (reserved "Bool" >> return iTBool) <|>
            (reserved "String" >> return iTString)  <|>
            (reserved "Date" >> return iTDate)  <|>
            (reserved "Time" >> return iTTime)  <|>
            (reserved "DateTime" >> return iTDateTime)  <|>
            (reserved "Real" >> return iTReal)  <|>
            (reserved "Duration" >> return iTDuration)  <|>
            liftM iTRecord recordIdentifier
      tp' <- entity tp
      return (tp', show $ renderTerm tp)
    entity :: Type -> Parser Type
    entity tp = try (reserved "entity" >> return (iTEnt tp)) <|>
                try (reserved "entities" >> return (iTEnt tp)) <|>
                return tp
    fieldName :: String -> Parser FieldName
    fieldName fieldType =
      option (toLower (head fieldType) : tail fieldType) propertyWithName
    propertyWithName :: Parser FieldName
    propertyWithName = do
      reserved "called" <|> reserved "named"
      fieldIdentifier

recordIdentifier :: Parser RecordName
recordIdentifier = lexeme (do
  c <- upper
  cs <- many (letter <|> digit)
  return $ c : cs) <?> "record name"

fieldIdentifier :: Parser FieldName
fieldIdentifier = identifier <?> "field name"

reportIdentifier :: Parser String
reportIdentifier = lexeme (do
  c <- upper
  cs <- many (letter <|> digit)
  return $ c : cs) <?> "report name"
