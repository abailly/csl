{-# LANGUAGE TemplateHaskell, TypeOperators #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Rules
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Morten Ib Nielsen
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module is a wrapper for the rule engine.
--
--------------------------------------------------------------------------------
module Poets.Rules
    (
     RuleEngine,
     createRuleEngine,
     stopRuleEngine,
     QueryValue(..),
     QueryError(..),
     POETSQValue,
     query,
     iQVar,
     iUnknown
    ) where


import qualified Data.Map as Map
import Data.Comp hiding (query)
import Data.Comp.Derive
import Poets.Config
import Poets.EventLog (EventLog)
import Poets.Data hiding (RunTimeError, ParseError, ValidationError, query)
--import Poets.Logging
--import System.Directory
--import System.FilePath.Posix ((</>))


--------------------------------------------------------------------------------
-- Rule Engine
--------------------------------------------------------------------------------

-- |A handle to the rule engine.
data RuleEngine = RuleEngine
    {
      -- |The event log.
      ruleEngineEventLog :: EventLog,
      -- |The rules to query against
      docs :: Map.Map RecordName Document
    }

type Document = Map.Map RecordName () --HACK

type POETSQValue = Term (QueryValue :+: Val) -- ^POETS query value.

data QueryValue e = QVar String -- ^Used for unknown values the engine should infer.
                  | Unknown String -- ^Used for unknown (non query target) values.

$(derive [makeFunctor, makeFoldable, makeTraversable,makeEqF,
          makeOrdF, smartConstructors]
         [''QueryValue] )


-- |Create a handle to the rule engine.
createRuleEngine :: RuleConfig -- ^Rule engine configuration.
                 -> EventLog
                 -> IO RuleEngine
createRuleEngine _ eventLog = do
{-  infoRU "Starting rule engine"
  rulesDir <- liftM (</> ruleRepository ruleConf) getCurrentDirectory
  debugRU $ "Reading rule sets from " ++ show rulesDir
--  docs <- readDocs rulesDir recordEnv-}
  let docs = Map.empty --REMOVE HACK TODO
{-  debugRU $ "Reading rule sets succeeded (" ++ show (Map.size docs) ++
            " rule sets parsed)"
  debugRU "Rule engine successfully started"-}
  return RuleEngine { ruleEngineEventLog = eventLog,
                      docs = docs
                    }

-- |Shutdown the rule engine. (Trivial, since the engine has no state.)
stopRuleEngine :: RuleEngine -> IO ()
stopRuleEngine _ = return ()


--------------------------------------------------------------------------------
-- Query
--------------------------------------------------------------------------------
type Solutions = [Map.Map String POETSQValue] -- ^Solutions (substitutions) have been found.

data QueryError = RunTimeError String -- ^Unexpected runtime error (i.e., \"this should not happen\").
                | Inconsistent -- ^The (partial) value provided is inconsistent with the rule set.
                | NoRuleSets -- ^The engine has no rule sets, to which the query in question can be applied.
                | CannotComplete -- ^The rule engine cannot answer the query (no rules apply).
                | PleaseSpecify [POETSQValue] -- ^The rule engine needs additional information.

instance Show QueryError where
    show (RunTimeError s) = "Runtime error: " ++ s
    show Inconsistent = "The query is inconsistent"
    show NoRuleSets = "There are no rule sets that apply to the query"
    show CannotComplete = "No rules are applicable"
    show (PleaseSpecify _) = "Please specify additional information"

query :: RuleEngine -- ^Rule engine handle.
      -> POETSQValue -- ^The query to execute.
      -> IO (Either QueryError Solutions)
query _ _ = return (Left $ RunTimeError "Rule engine not implemented")