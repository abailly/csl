{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Contracts.Language.CSL.Parser
-- Copyright   :  3gERP, 2012
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- Parsing of CSL.
--
--------------------------------------------------------------------------------
module Poets.Contracts.Language.CSL.Parser
    (
     parseContract,
     parseFunctionsFile,
     parseFunctions,
     parseFunction,
     parseClauseDef,
     parseClause,
     parseExpr,
     ParseError,
     SourceName
    ) where

import           Control.Monad.Identity
import           Data.Either
import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import           Poets.Contracts.Base
import           Poets.Contracts.Language.CSL.AST
import           Poets.Data                       hiding (Type, proj)
import           Prelude                          hiding (EQ, GT, LT, exp)
import           Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Language
import           Text.Parsec.String
import qualified Text.Parsec.Token                as P

-- |Type for an operator declaration in the 'Parser' monad.
type Op = Operator String () Identity

-- |Definition of the general lexer.
lexer :: P.TokenParser ()
lexer = P.makeTokenParser
        P.LanguageDef
        {
          P.commentStart = "/*",
          P.commentEnd = "*/",
          P.commentLine = "//",
          P.nestedComments = True,
          P.identStart = lower,
          P.identLetter = alphaNum <|> oneOf "_'",
          P.opStart = P.opStart haskellStyle,
          P.opLetter = P.opLetter haskellStyle,
          P.caseSensitive = True,
          P.reservedOpNames = resOpNames,
          P.reservedNames = resNames ++ resOpNames
        }

-- |CSL operator keywords.
resOpNames :: [String]
resOpNames = [".", "and", "or"] ++
             map show [NEQ, GEQ, LT, GT, MINUS, DMINUS, OR] ++
             map show [EQ, LEQ, PLUS, TIMES, DIV, AND,
                       CONS, DPLUS, DTIMES] ++
             [show UMINUS, show NOT]

-- |CSL keywords.
resNames :: [String]
resNames = [":", "=", "|", "->", "()", "<<", ">>"] ++
           -- Types
           ["Int", "Bool", "String", "Date", "Time"] ++
           ["DateTime", "Real", "Duration", "()"] ++
           -- Keywords and reserved names
           ["name", "type", "contract", "clause", "true", "false", "when"] ++
           ["if", "then", "else"] ++
           ["case", "of", "where", "due", "immediately", "within"] ++
           ["after", "fun", "val", "remaining", "let", "in"] ++
           ["fulfilment"]

getPos = liftM Just getPosition


--------------------------------------------------------------------------------
-- alias definitions for the lexer
--------------------------------------------------------------------------------

whiteSpace = P.whiteSpace lexer
parens = P.parens lexer
float = P.float lexer
natural = P.natural lexer
reservedOp = P.reservedOp lexer
reserved = P.reserved lexer
symbol = P.symbol lexer
lexeme = P.lexeme lexer
braces = P.braces lexer
brackets = P.brackets lexer
angles = P.angles lexer
identifier = P.identifier lexer
commaSep = P.commaSep lexer
commaSep1 = P.commaSep1 lexer


--------------------------------------------------------------------------------
-- CSL clause/contract parsers
--------------------------------------------------------------------------------

-- |Parse a full contract, i.e., a set of function definitions, a set of
-- clause definitions, and the contract definition itself.
parseContract :: SourceName
              -> String -- ^String to parse.
              -> Either ParseError ContractDefPos
parseContract = parseInput contract

-- |Parse a set of function definitions from the supplied file.
parseFunctionsFile :: FilePath -> IO (Either ParseError [FunctionDefPos])
parseFunctionsFile path = do
  cont <- readFile path
  return $ parseFunctions path cont

-- |Parse a set of function definitions.
parseFunctions :: SourceName -> String -> Either ParseError [FunctionDefPos]
parseFunctions = parseInput $ many functionDefinition

-- |Parse a single function definition.
parseFunction :: SourceName -> String -> Either ParseError FunctionDefPos
parseFunction = parseInput functionDefinition

-- |Parse a single clause definitions.
parseClauseDef :: SourceName -> String -> Either ParseError ClausePosDef
parseClauseDef = parseInput clauseDefinition

-- |Parse a clause.
parseClause :: SourceName -> String -> Either ParseError ClausePos
parseClause = parseInput clause

-- |Parse an expression.
parseExpr :: SourceName -> String -> Either ParseError ExprPos
parseExpr = parseInput exp

-- |Contract parser, i.e., a set of function definitions, a set of clause
-- definitions, and the contract definition itself.
contract :: Parser ContractDefPos
contract = do
  reserved "name"
  reserved ":"
  n <- contractIdent
  reserved "type"
  reserved ":"
  tp <- recordIdent
  reserved "description"
  reserved ":"
  desc <- stringLit
  defs <- many (fmap Left functionDefinition <|> fmap Right clauseDefinition)
  let (fDefs, cDefs) = partitionEithers defs
  reserved "contract"
  reserved "="
  c <- clause
  return ContractDefinition{contractDefName = n,
                            contractDefType = tp,
                            contractDefDescription = desc,
                            contractDefFunDefs = fDefs,
                            contractDefClauseDefs = defsToMap cDefs,
                            contractDefBody = c}
    where defsToMap :: [ClausePosDef] -> Map TemplateName ClausePosDef
          defsToMap = foldr (\d -> Map.insert (clauseDefName d) d) Map.empty

-- |Function definition parser.
functionDefinition :: Parser FunctionDefPos
functionDefinition = lexeme $ valDef <|> funDef
    where funDef :: Parser FunctionDefPos
          funDef = do
            reserved "fun"
            funName <- functionIdent
            funParams <- many1 varIdent <?> "parameter list"
            reserved "="
            pos <- getPos
            body <- exp
            let def = foldr (iAELambda pos) body funParams
            return FunctionDefinition{functionName = funName,
                                      functionExp = def}
          valDef :: Parser FunctionDefPos
          valDef = do
            reserved "val"
            bindName <- varIdent
            reserved "="
            body <- exp
            return FunctionDefinition{functionName = bindName,
                                      functionExp = body}

-- |Clause template parser.
clauseDefinition :: Parser ClausePosDef
clauseDefinition = lexeme $ do
  reserved "clause"
  t <- templateIdent
  params <- parens (commaSep param) <?> "clause argument list"
  aparams <- angles (commaSep aparam)  <?> "clause party list"
  reserved "="
  c <- clause
  return ClauseDefinition{clauseDefName = t,
                          clauseDefParams = params,
                          clauseDefPartyParams = aparams,
                          clauseDefBody = c}
      where param :: Parser (Var,Type)
            param = do
              x <- varIdent
              reserved ":"
              t <- typeIdent
              return (x,t)
            aparam :: Parser (Var,Type)
            aparam = do
              x <- varIdent
              reserved ":"
              t <- typeIdent
              return (x,t)

-- |Clause parser.
clause :: Parser ClausePos
clause = buildExpressionParser clauseOps clauseFactor <?> "clause"

-- |Declaration of clause operators.
clauseOps :: [[Op ClausePos]]
clauseOps = [[bin "and" iAAnd, bin "or" iAOr]]
    where
      -- Parsing a binary left associative infix operator
      bin :: String -> (SrcPos -> ClausePos -> ClausePos -> ClausePos) -> Op ClausePos
      bin op f = Infix (do pos <- getPos
                           reservedOp op
                           return $ f pos)
                       AssocLeft

-- |Supplementary parser for clauses.
clauseFactor :: Parser ClausePos
clauseFactor = (parens clause <?> "grouping") <|>
               (fulfilmentClause <?> "fulfilment")  <|>
               (obligationClause <?> "obligation") <|>
               (externalChoiceClause <?> "external choice") <|>
               (internalChoiceClause <?> "internal choice") <|>
               (caseClause <?> "case split") <|>
               (instantiateClause <?> "template instantiation")
    where exp' :: Parser ClausePos
          exp' = undefined -- fmap deepInject4 exp
          pexp' :: Parser ClausePos
          pexp' = undefined -- fmap deepInject4 pexp
          -- The trivially fulfiled clause
          fulfilmentClause :: Parser ClausePos
          fulfilmentClause = do
            pos <- getPos
            reserved "fulfilment"
            return $ iAFulfilment pos
          -- Obligation clause
          obligationClause :: Parser ClausePos
          obligationClause = do
            pos <- getPos
            pe <- angles pexp'
            trType <- transactionType
            b <- binders
            pred <- predicate
            dead <- deadline
            x <- remainderVar
            cont <- continuation "then"
            return $ inject $
               ObligationSug{
                 clauseSugResponsible = pe,
                 clauseSugTransactionType = trType,
                 clauseSugBinders = b,
                 clauseSugRemainderVar = x,
                 clauseSugPredicate = pred,
                 clauseSugDeadline = dead,
                 clauseSugContinuation = cont} :&: pos
          -- External choice clause
          externalChoiceClause :: Parser ClausePos
          externalChoiceClause = do
            pos <- getPos
            reserved "when"
            trType <- transactionType
            b <- binders
            pred <- predicate
            dead <- deadline
            x <- remainderVar
            cont <- continuation "then"
            cont' <- continuation "else"
            return $ inject $
               ExternalChoiceSug{
                 clauseSugTransactionType = trType,
                 clauseSugBinders = b,
                 clauseSugRemainderVar = x,
                 clauseSugPredicate = pred,
                 clauseSugDeadline = dead,
                 clauseSugContinuation = cont,
                 clauseSugElse = cont'} :&: pos
          -- Internal choice
          internalChoiceClause :: Parser ClausePos
          internalChoiceClause = do
            pos <- getPos
            reserved "if"
            e <- exp'
            reserved "then"
            c1 <- clause
            reserved "else"
            c2 <- clause
            return $ iAInternalChoice pos e c1 c2
          -- Case clause
          caseClause :: Parser ClausePos
          caseClause = do
            pos <- getPos
            reserved "case"
            e <- exp'
            tp <- optionMaybe (reserved ":" >> recordIdent)
            reserved "of"
            case1 <- caseC
            cases <- many $ try (reserved "|" >> caseC)
            return $ iACase pos e tp (case1 : cases)
          caseC :: Parser (ClauseCase ClausePos)
          caseC = do
            rName <- recordIdent
            var <- varIdent
            reserved "->"
            c <- clause
            return ClauseCase{clauseCaseRecordName = rName,
                              clauseCaseVar = var,
                              clauseCaseBody = c}
          -- Clause template instantiation
          instantiateClause :: Parser ClausePos
          instantiateClause = do
            pos <- getPos
            tName <- templateIdent
            args <- parens (commaSep exp')
            partyArgs <- angles (commaSep pexp')
            return $ iAInstantiate pos tName args partyArgs
          -- The transaction type of an obligation/external choice
          transactionType :: Parser TransactionType
          transactionType = recordIdent <?> "transaction type"
          -- A list of bound variables w/ associated field names
          binders :: Parser [(Var,FieldName)]
          binders = option [] $ parens (commaSep1 binder)
          binder :: Parser (Var,FieldName)
          binder = do
            f <- fieldIdent
            x <- varIdent
            return (x,f)
          -- Remainder variable
          remainderVar :: Parser (Maybe Var)
          remainderVar = optionMaybe (reserved "remaining" >> varIdent)
          -- The "where" clause of obligations/external choices
          predicate :: Parser (Maybe ClausePos)
          predicate = optionMaybe (reserved "where" >> exp')
          -- The deadline of obligations/external choices
          deadline :: Parser (DeadlineSug ClausePos)
          deadline = do
            reserved "due"
            (do reserved "immediately"
                e <- optionMaybe (reserved "after" >> exp')
                return Immediately{deadlineSugAfterM = e}) <|>
             (do reserved "within"
                 e1 <- exp'
                 e2 <- optionMaybe (reserved "after" >> exp')
                 return Within{deadlineSugWithin = e1,
                               deadlineSugAfterM = e2}) <|>
             (do reserved "after"
                 e <- exp'
                 return After{deadlineSugAfter = e})
          -- Parsing of then/else branch
          continuation :: String -> Parser (Maybe ClausePos)
          continuation key = optionMaybe (reserved key >> clause)


--------------------------------------------------------------------------------
-- Expression parser
--------------------------------------------------------------------------------

-- |Party expression parser.
pexp :: Parser ExprPos
pexp = buildExpressionParser pexpOps expFactor <?> "party expression"

-- |Declaration of party expression operators.
pexpOps :: [[Op ExprPos]]
pexpOps = [[Postfix (try proj), Postfix (try update)],
           [apply],
           [unSugar UMINUS, unSugar NOT],
           [bin TIMES,bin DIV,bin DTIMES],
           [bin PLUS,binSugar MINUS,bin DPLUS,binSugar DMINUS],
           [bin' CONS],
           [bin EQ, binSugar NEQ],
           [bin AND, binSugar OR]]

-- |Expression parser.
exp :: Parser ExprPos
exp = buildExpressionParser expOps expFactor

-- |Declaration of expression operators.
expOps :: [[Op ExprPos]]
expOps = [[Postfix (try proj), Postfix (try update)],
          [apply],
          [unSugar UMINUS, unSugar NOT],
          [bin TIMES, bin DIV, bin DTIMES],
          [bin PLUS, binSugar MINUS, bin DPLUS, binSugar DMINUS],
          [bin' CONS],
          [bin EQ, binSugar NEQ, bin LEQ, binSugar GEQ,
           binSugar LT, binSugar GT],
          [bin AND, binSugar OR]]

-- Field projection
proj = do
  pos <- getPos
  reservedOp "."
  f <- fieldIdent
  return $ iAEProj pos f

-- Field update
update = do
  pos <- getPos
  fes <- braces $ commaSep1 $ do
           f <- fieldIdent
           reserved "="
           e <- exp
           return (f,e)
  return $ \e -> iAESUpdate pos e fes

-- Parsing an unary augared operator
unSugar :: UnOpSug -> Op ExprPos
unSugar o = Prefix $ do pos <- getPos
                        reservedOp $ show o
                        return $ iAESUnOp pos o

-- Parsing a binary left associative infix operator
bin :: BinOp -> Op ExprPos
bin o = Infix (do pos <- getPos
                  reservedOp $ show o
                  return $ iAEBinOp pos o)
              AssocLeft

-- Parsing a binary right associative infix operator
bin' :: BinOp -> Op ExprPos
bin' o = Infix (do pos <- getPos
                   reservedOp $ show o
                   return $ iAEBinOp pos o)
               AssocRight

-- Parsing a binary left associative infix sugared operator
binSugar :: BinOpSug -> Op ExprPos
binSugar o = Infix (do pos <- getPos
                       reservedOp $ show o
                       return $ iAESBinOp pos o)
                   AssocLeft

-- Parsing of function application
apply :: Op ExprPos
apply = Infix (liftM iAEApply getPos) AssocLeft

-- |Supplementary parser for expressions.
expFactor :: Parser ExprPos
expFactor = lexeme $
   (boolean <?> "Boolean") <|>
   (unit <?> "unit") <|>
   (parens exp <?> "grouping") <|>
   (dateOrTime <?> "date/time") <|>
   (try duration <?> "duration") <|>
   (try real <?> "real") <|>
   (nat <?> "natural number") <|>
   (strng <?> "string") <|>
   recordOrEntity <|>
   (lambdaabs <?> "lambda abstraction") <|>
   (ifthenelse <?> "if-then-else") <|>
   (caseexp <?> "case expression") <|>
   (letexp <?> "let expression") <|>
   (list <?> "list") <|>
   (var <?> "variable")
    where boolean = do
            pos <- getPos
            fmap (iAVBool pos) ((reserved "true" >> return True) <|>
                                (reserved "false" >> return False))
          unit = do
            pos <- getPos
            reserved "()"
            return (iAVUnit pos)
          dateOrTime = do
            pos <- getPos
            try $ symbol "<<"
            e <- dateTime pos <|> fmap (iAVTime pos) timePart
            symbol ">>"
            return e
          dateTime pos = do
            d <- datePart
            option (iAVDate pos d)
                   (do char ' '
                       fmap (iAVDateTime pos . createDateTime d) timePart)
          datePart :: Parser Date
          datePart = do
            year :: Integer <- try (fmap read $ count 4 digit) <?> "year"
            char '-'
            month :: Int <- fmap read (count 2 digit) <?> "month"
            char '-'
            day :: Int <- fmap read (count 2 digit) <?> "day"
            maybe (fail "Failed to parse date")
                  return
                  (createDate year month day)
          timePart :: Parser Time
          timePart = do
            hour :: Int <- fmap read (count 2 digit) <?> "hour"
            char ':'
            minute :: Int <- fmap read (count 2 digit) <?> "minute"
            char ':'
            second :: Int <- fmap read (count 2 digit) <?> "second"
            microsecond :: Int <- option 0 (char '.' >> fmap read (count 6 digit)) <?> "microsecond"
            maybe (fail "Failed to parse time")
                  return
                  (createTime hour minute second microsecond)
          duration = do
            pos <- getPos
            let d0 = fromSeconds 0
            n <- natural
            -- Seconds, minutes, hours, days, weeks, months, or years
            t <- char 'S' <|> char 'm' <|> char 'H' <|> char 'D' <|>
                 char 'W' <|> char 'M' <|> char 'Y'
            fmap (iAVDuration pos) $
                 case t of
                   'S' -> return d0{durationSeconds = fromInteger n}
                   'm' -> return d0{durationMinutes = fromInteger n}
                   'H' -> return d0{durationHours = fromInteger n}
                   'D' -> return d0{durationDays = fromInteger n}
                   'W' -> return d0{durationWeeks = fromInteger n}
                   'M' -> return d0{durationMonths = fromInteger n}
                   'Y' -> return d0{durationYears = fromInteger n}
                   _   -> fail $ "Unknown duration character '" ++ [t] ++ "'"
          real = do
            pos <- getPos
            fmap (iAVReal pos) float
          nat = do
            pos <- getPos
            fmap (iAVInt pos . fromInteger) natural
          strng = do
            pos <- getPos
            fmap (iAVString pos) stringLit
          recordOrEntity = do
            pos <- getPos
            rn <- recordIdent
            (entity pos rn <?> "entity") <|> (record pos rn <?> "record")
          entity pos rn = do
            refId <- angles natural
            return $ iAVEnt pos VEntity{ventType = rn,
                                        ventId = fromInteger refId,
                                        ventContext = Nothing}
          record pos rn = do
            fields <- option [] $ braces $ commaSep1 fieldExp
            return $ iAVRecord pos VR{vrecordName = rn,
                                      vrecordFields = newFields fields}
          fieldExp = do
            f <- fieldIdent
            reserved "="
            e <- exp
            return VF{vfieldName = f, vfieldValue = e}
          lambdaabs = do
            pos <- getPos
            symbol "\\"
            params <- many1 varIdent
            reserved "->"
            body <- exp
            return $ iAESLambda pos params body
          ifthenelse = do
            pos <- getPos
            reserved "if"
            boolExp <- exp
            reserved "then"
            thenExp <- exp
            reserved "else"
            elseExp <- exp
            return $ iAEIfThenElse pos boolExp thenExp elseExp
          caseexp = do
            pos <- getPos
            reserved "case"
            e <- exp
            tp <- optionMaybe (reserved ":" >> recordIdent)
            reserved "of"
            case1 <- caseE
            cases <- many (reserved "|" >> caseE)
            return $ iAECase pos e tp (case1 : cases)
          caseE :: Parser (CaseExp ExprPos)
          caseE = do
            rName <- recordIdent
            var <- varIdent
            reservedOp "->"
            e <- exp
            return CaseExp{caseExpRecordName = rName,
                           caseExpVar = var,
                           caseExpBody = e}
          letexp = do
            pos <- getPos
            reserved "let"
            var <- varIdent
            reserved "="
            e1 <- exp
            reserved "in"
            e2 <- exp
            return $ iAESLet pos var e1 e2
          list = do
            pos <- getPos
            liftM (iAVList pos) $ brackets (commaSep exp)
          var = do
            pos <- getPos
            v <- varIdent
            return $ iAEVar pos v

-- |String parser.
stringLit :: Parser String
stringLit = lexeme $ do
  char '"'
  s <- many (many1 (noneOf "\\\"\r\n") <|> quotedPair)
  char '"'
  return $ concat s
      where quotedPair :: Parser String
            quotedPair = do
              char '\\'
              r <- noneOf "\r\n"
              return ['\\',r] <?> "quoted pair"

-- |Parser for variables.
varIdent :: Parser Var
varIdent = lexeme (identifier <?> "variable name")

-- |Parser for function names.
functionIdent :: Parser Var
functionIdent = lexeme (identifier <?> "function name")

-- |Parser for types.
typeIdent :: Parser Type
typeIdent = buildExpressionParser typeOps typeFactor

-- |Type operators.
typeOps :: [[Op Type]]
typeOps = [[fun]]
    where fun = Infix (reservedOp "->" >> return iTFunction) AssocRight

-- |Supplementary parser for types.
typeFactor :: Parser Type
typeFactor = lexeme $
  parens typeIdent <|>
  ((reserved "Int" >> return iTInt) <?> "Int") <|>
  ((reserved "Bool" >> return iTBool) <?> "Bool") <|>
  ((reserved "String" >> return iTString) <?> "String") <|>
  ((reserved "Date" >> return iTDate) <?> "Date") <|>
  ((reserved "Time" >> return iTTime) <?> "Time") <|>
  ((reserved "DateTime" >> return iTDateTime) <?> "DateTime") <|>
  ((reserved "Real" >> return iTReal) <?> "Real") <|>
  ((reserved "Duration" >> return iTDuration) <?> "Duration") <|>
  ((reserved "()" >> return iTUnit) <?> "unit type") <|>
  (fmap iTList (brackets typeIdent) <?> "list type") <|>
  (fmap iTEnt (angles typeIdent) <?> "reference type") <|>
  (fmap iTRecord recordIdent <?> "record name") <|>
  (fmap iTVar identifier <?> "type variable identifier")

-- |Parser for record names
recordIdent :: Parser RecordName
recordIdent = capString <?> "record name"

-- |Parser for contract template names
contractIdent :: Parser RecordName
contractIdent = lexeme (many letter) <?> "contract template name"

-- |A capitalized string.
capString :: Parser String
capString = lexeme $ do head <- upper
                        tail <- many letter
                        return $ head:tail

-- |Parser for field names.
fieldIdent :: Parser FieldName
fieldIdent = lexeme (identifier <?> "field name")

-- |Parser for clause template names
templateIdent :: Parser TemplateName
templateIdent = lexeme (identifier <?> "template name")

parseInput :: Parser a -> SourceName -> String -> Either ParseError a
parseInput p = parse p'
    where p' = whiteSpace >> p >>= \res -> eof >> return res
