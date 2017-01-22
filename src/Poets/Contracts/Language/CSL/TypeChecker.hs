{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Contracts.Languge.CSL.TypeChecker
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module implements type checking of CSL contract definitions.
--
--------------------------------------------------------------------------------
module Poets.Contracts.Language.CSL.TypeChecker
    (
     typeCheckContractDefinition
    ) where

import           Control.Monad.Error                             hiding
                                                                  (sequence_)
import           Control.Monad.Identity                          hiding
                                                                  (sequence_)
import           Control.Monad.Reader                            hiding
                                                                  (sequence_)
import           Data.Comp.Ops
import           Data.Foldable                                   (Foldable)
import           Data.Foldable                                   (sequence_)
import           Data.List
import           Data.Map                                        (Map)
import qualified Data.Map                                        as Map
import           Data.Set                                        (Set)
import qualified Data.Set                                        as Set
import           Poets.Contracts.Base
import           Poets.Contracts.Language.CSL.AST
import           Poets.Contracts.Language.CSL.Exceptions
import           Poets.Contracts.Language.CSL.Render             ()
import           Poets.Contracts.Language.CSL.Typing.Inference
import           Poets.Contracts.Language.CSL.Typing.TypeInferer hiding (srcPos,
                                                                  variableEnv)
import           Poets.Data                                      hiding (Type)
import           Poets.EventLog.Names                            (contractEventFieldContractId)
import           Prelude                                         hiding
                                                                  (sequence_,
                                                                  showList)

-- |The type checking monad. Clause/function definition typings are stored in
-- the environment.
type TM = ReaderT Environment (ErrorT CSLError Identity)

-- |Run a computation in the type checker monad.
evalTM :: Environment -> TM a -> Either CSLError a
evalTM env m = runIdentity (runErrorT $ runReaderT m env)

-- |The environment used during type checking.
data Environment = Environment{
      -- |The typing environment for variables.
      variableEnv :: VarEnv,
      -- |The typing environment for clause definitions.
      clauseEnv   :: ClauseEnv,
      -- |The typing environment for record definitions.
      recordEnv   :: RecEnv,
      -- |The entity typing environment.
      entityEnv   :: EntityTypingEnv,
      -- |Source code information.
      srcPos      :: SrcPos
    }

-- |Clause typing environment. Each template name is associated with a list of
-- typed parameters and a list of typed party parameters.
type ClauseEnv = Map TemplateName ([Type],[Type])

-- |Extend the variable typing environment.
withVars :: VarEnv -> TM a -> TM a
withVars vars = local $ \e -> e{variableEnv = Map.union vars $ variableEnv e}

-- |Update source position in the environment.
withSrcPos :: SrcPos -> TM a -> TM a
withSrcPos pos = local $ \e -> e{srcPos = pos}

-- |Extend the clause typing environment.
withClauseEnv :: ClauseEnv -> TM a -> TM a
withClauseEnv cEnv = local $ \e -> e{clauseEnv = cEnv}

-- |Lookup a clause typing in the environment.
lookupClauseDefinition :: TemplateName -> TM ([Type],[Type])
lookupClauseDefinition t = do
  mtdef <- asks (Map.lookup t . clauseEnv)
  maybe (clauseDefUndefError t) return mtdef

-- |Build a variable typing environment from a transaction type and an
-- association of variables with fields of that type.
buildEnv :: TransactionType -> [(Var,FieldName)] -> TM VarEnv
buildEnv trType b = do
  recordEnv <- asks recordEnv
  fEnv <- either typeError return $ getTypeFields recordEnv trType
  let vars = map fst b
  let fields = map snd b
  let dupFields = nub [f | f <- fields, length (filter (== f) fields) > 1]
  let dupVars = nub [x | x <- vars, length (filter (== x) vars) > 1]
  let undefFields = [f | f <- fields,
                         either (const True) (const False)
                                (getFieldInfo fEnv f)]
  -- Check if declaration contains duplicate fields
  unless (null dupFields) (duplicateFieldsError dupFields)
  -- Check if declaration contains duplicate variables
  unless (null dupVars) (duplicateVarsError dupVars)
  -- Check if declaration contains unknown fields
  unless (null undefFields) (undefFieldsError undefFields trType)
  -- Map each variable to the type of the associated field
  return $ Map.map (\f -> let Right finfo = getFieldInfo fEnv f in
                          simpleTyping $ fieldType finfo)
                   (Map.fromList b)

-- |The type checker class.
class TypeC f g where
    typeC :: f (Term g) -> (TM (Set (Either Var Party)))

-- |Type check a clause. The returned set is the set of parties occurring in the
-- clause.
typeCheckClause :: TypeC f f => Term f -> TM (Set (Either Var Party))
typeCheckClause = typeC . unTerm

instance (TypeC g g,
          (Val :&: SrcPos) :<: g,
          (VUnit :&: SrcPos) :<: g,
          (CoreExp :&: SrcPos) :<: g) => TypeC (CoreClause :&: SrcPos) g where
    typeC (c :&: pos) = withSrcPos pos $ case c of
            Fulfilment{} ->
              -- Trivially well-typed
              return Set.empty
            Obligation{clauseResponsible = resp,
                       clauseTransactionType = trType,
                       clauseBinders = b,
                       clauseRemainderVar = rVar,
                       clausePredicate = pred,
                       clauseDeadline = deadline,
                       clauseContinuation = c'} -> do
              party <- typeCheckPartyExpr resp
              vEnv <- buildEnv trType b
              -- Type check predicate in the updated variable environment
              withVars vEnv $ typeCheckPredicate pred
              -- Type check deadline in the original variable environment
              typeCheckDeadline deadline
              -- Type check contract continuation in the updated variable
              -- environment, including the remainder variable
              parties <- withVars (Map.insert rVar (simpleTyping iTDuration)
                                              vEnv)
                                  (typeCheckClause c')
              return $ Set.insert party parties
            ExternalChoice{clauseTransactionType = trType,
                           clauseBinders = b,
                           clauseRemainderVar = rVar,
                           clausePredicate = pred,
                           clauseDeadline = deadline,
                           clauseContinuation = c1,
                           clauseElse = c2} -> do
              vEnv <- buildEnv trType b
              -- Type check predicate in the updated variable environment
              withVars vEnv $ typeCheckPredicate pred
              -- Type check deadline in the original variable environment
              typeCheckDeadline deadline
              -- Type check contract continuation in the updated variable
              -- environment, including the remainder variable
              parties1 <- withVars (Map.insert rVar (simpleTyping iTDuration)
                                               vEnv)
                                   (typeCheckClause c1)
              -- Type check else branch in the original variable environment
              parties2 <- typeCheckClause c2
              return $ parties1 `Set.union` parties2
            InternalChoice{clauseCondition = e,
                           clauseThen = c1,
                           clauseElse = c2} -> do
              -- First check that the conditional expression has boolean type
              typeCheckPredicate e
              parties1 <- typeCheckClause c1
              parties2 <- typeCheckClause c2
              return $ parties1 `Set.union` parties2
            And{clauseLeft = c1, clauseRight = c2} -> do
              parties1 <- typeCheckClause c1
              parties2 <- typeCheckClause c2
              return $ parties1 `Set.union` parties2
            Or{clauseLeft = c1, clauseRight = c2} -> do
              parties1 <- typeCheckClause c1
              parties2 <- typeCheckClause c2
              let parties = parties1 `Set.union` parties2
              -- Check that the party in both sub clauses is the same
              if Set.size parties > 1 then
                  disjunctionPartiesError (Set.toList parties1)
                                          (Set.toList parties2)
              else
                  return parties
            Case{clauseCaseExpr = e, clauseCaseType = mrName, clauseCases = cs} -> do
              recordEnv <- asks recordEnv
              -- The list of records names which are covered by a case
              let rNames = map clauseCaseRecordName cs
              -- Check for exhaustiveness
              rName <- checkExhaustiveness recordEnv rNames mrName
              -- The case expression must have the type which the cases cover
              typeCheckExprTM e $ iTRecord rName
              -- Type check each case in the clause
              partiess <- mapM typeCheckCase cs
              return $ Set.unions partiess
                  where checkExhaustiveness recordEnv rNames mrName = do
                          case mrName of
                            Just rName ->
                                -- Explicit type annotation
                                checkEx recordEnv rName rNames
                            Nothing -> do
                                -- No type annotation, so a super type must be
                                -- inferred
                                supers <- superTypes recordEnv rNames
                                case supers of
                                  [rName] ->
                                       checkEx recordEnv rName rNames
                                  _ ->
                                       caseCommonSuperTypeError "Unable to infer a (unique) super type"
                        checkEx recordEnv rName rNames = do
                          case uncovered recordEnv rName rNames of
                            Left e   -> typeError e
                            Right [] -> return rName
                            Right u  -> nonExhaustivePatterns u
                        superTypes recordEnv rNames =
                          foldM (\r2s r1 -> do
                                   supers <- mapM (either caseCommonSuperTypeError return .
                                                   findCommonSuperTypes recordEnv r1) r2s
                                   return $ concat supers)
                                [head rNames]
                                rNames
                        typeCheckCase ClauseCase{clauseCaseRecordName = rName,
                                                 clauseCaseVar = x,
                                                 clauseCaseBody = c} =
                          -- Type check a clause in the case split
                          withVars (Map.singleton x $ simpleTyping $
                                                      iTRecord rName)
                                   (typeCheckClause c)
            Instantiate{clauseClauseTemplate = tName,
                        clauseArgs = args,
                        clausePartyArgs = partyArgs} -> do
              (paramTypes, partyParamTypes) <- lookupClauseDefinition tName
              -- Check first that the number of arguments is correct
              when (length args /= length paramTypes)
                   (templateArgsError tName (length paramTypes) (length args))
              -- Then check that the number of parties is correct
              when (length partyArgs /= length partyParamTypes)
                   (templatePartyArgsError tName (length partyParamTypes)
                                                 (length partyArgs))
              -- Then type check each argument against the expected type
              mapM_ (uncurry typeCheckExprTM) $ zip args paramTypes
              -- Finally type check each party argument against the expected
              -- type
              mapM_ (uncurry typeCheckExprTM) $ zip partyArgs partyParamTypes
              parties <- mapM typeCheckPartyExpr partyArgs
              return $ Set.fromList parties

-- |The expressions in a deadline must be of type Duration.
typeCheckDeadline :: ((Val :&: SrcPos) :<: f,
                      (VUnit :&: SrcPos) :<: f,
                      (CoreExp :&: SrcPos) :<: f) => Deadline (Term f) -> TM ()
typeCheckDeadline Deadline{deadlineWithin = e1, deadlineAfter = e2} = do
  typeCheckExprTM e1 iTDuration
  typeCheckExprTM e2 iTDuration
  return ()

-- |A predicate expression must be of type Boolean.
typeCheckPredicate :: ((Val :&: SrcPos) :<: f,
                       (VUnit :&: SrcPos) :<: f,
                       (CoreExp :&: SrcPos) :<: f) => Term f -> TM ()
typeCheckPredicate p = typeCheckExprTM p iTBool

-- |Type check a clause definition. This conceptually amounts to type checking
-- the body of the definition in an extended typing environment.
typeCheckClauseDef :: ClauseCorePosDef -> TM ()
typeCheckClauseDef ClauseDefinition{clauseDefName = tName,
                                    clauseDefParams = params,
                                    clauseDefPartyParams = partyParams,
                                    clauseDefBody = c} =
  withSrcPos (snd $ projectA $ unTerm c) $ do
    let pVars = fst $ unzip partyParams
    let vars = fst (unzip params) ++ pVars
    let dupVars = nub [x | x <- vars, length (filter (== x) vars) > 1]
    -- First check if declaration contains duplicate variables
    unless (null dupVars) (templateDuplicateVarsError tName dupVars)
    -- Then check that the types of the template parameters are defined
    mapM_ checkParamType $ params ++ partyParams
    -- Then type check the body of the template
    parties <- withVars varEnv (typeCheckClause c)
    -- Finally check that the (parametrized) parties occuring in the body
    -- of the template are exactly those in the signature of the template
    when (parties /= Set.fromList (map Left pVars))
         (templatePartyMismatch tName pVars (Set.toList parties))
    where checkParamType :: (Var,Type) -> TM ()
          checkParamType (_,tp) = do
            recordEnv <- asks recordEnv
            cata (checkType recordEnv) tp
          checkType :: (TypeConstant :<: t, Foldable t) => RecEnv
                    -> t (TM ())
                    -> TM ()
          checkType recordEnv tp =
              case proj tp of
                Just (TRecord r) -> unless (isDefined recordEnv r)
                                           (templateUndefinedRecord tName r)
                _ -> sequence_ tp
          -- Typing environment for value + party parameters
          varEnv = foldr (\(x,tp) -> Map.insert x (simpleTyping tp))
                         Map.empty
                         (params ++ partyParams)

-- |Type check a contract definition. This amounts to (1) do type inference for
-- function definitions, (2) type check all (mutually recursive) clause
-- definitions, and (3) type check the body of the contract definition.
typeCheckContractDef :: ContractDefCorePos -> TM ()
typeCheckContractDef ContractDefinition{contractDefName = tName,
                                        contractDefType = tType,
                                        contractDefClauseDefs = cDefs,
                                        contractDefFunDefs = fDefs,
                                        contractDefBody = c} =
  withSrcPos (snd $ projectA $ unTerm c) $ do
    recordEnv <- asks recordEnv
    entityEnv <- asks entityEnv
    -- First check that the template type is in fact a contract type
    either (const $ contractDefIllegalType tName tType)
           return
           (checkSubType recordEnv (iTRecord tType) (iTRecord contractClass))
    -- (1) Then perform type inference for function definitions
    vEnv <- asks variableEnv
    fEnv <- either throwError
                   return
                   (typeInferFunctionDefinitions recordEnv entityEnv vEnv fDefs)
    -- (2) Then type check each clause definition in the contract definition.
    -- Clause definitions can be (mutually) recursive, so we perform the
    -- type check in a clause typing environment containing all clause
    -- definitions
    withClauseEnv cEnv $ withVars fEnv $
                         mapM_ typeCheckClauseDef $ Map.elems cDefs
    -- (3) Then type check the body of the contract template, where the variable
    -- typing environment is extended to include the fields of the contract
    -- meta data, and the contract ID
    metaDataEnv <- getMetaDataEnv
    let varEnv = Map.insert contractEventFieldContractId
                            (simpleTyping iTInt)
                            (metaDataEnv `Map.union` fEnv)
    withClauseEnv cEnv $ withVars varEnv (typeCheckClause c)
    return ()
        where cEnv = Map.map (\t -> (snd $ unzip $ clauseDefParams t,
                                     snd $ unzip $ clauseDefPartyParams t))
                             cDefs
              -- Retrieves the fields defined in the contract meta data type,
              -- and their associated types
              getMetaDataFields :: TM (Map FieldName Type)
              getMetaDataFields = do
                recordEnv <- asks recordEnv
                either typeError
                       (return . Map.map fieldType . fieldEnvMap)
                       (getTypeFields recordEnv tType)
              -- Build the typing environment from the contract type
              getMetaDataEnv :: TM VarEnv
              getMetaDataEnv = do
                vEnv <- getMetaDataFields
                return $ Map.map simpleTyping vEnv

-- Dummy instance
instance TypeC (Val :&: SrcPos) g where
    typeC _ = unexptedConstructError "Val"

-- Dummy instance
instance TypeC (VUnit :&: SrcPos) g where
    typeC _ = unexptedConstructError "VUnit"

-- Dummy instance
instance TypeC (CoreExp :&: SrcPos) g where
    typeC _ = unexptedConstructError "CoreExp"


--------------------------------------------------------------------------------
-- Type checking (type inference) of CSL expressions
--------------------------------------------------------------------------------

-- |Check the type of a CSL expression inside the TM monad.
typeCheckExprTM :: ((Val :&: SrcPos) :<: f,
                    (VUnit :&: SrcPos) :<: f,
                    (CoreExp :&: SrcPos) :<: f) => Term f -> Type -> TM ()
typeCheckExprTM e expectedTp = do
  env <- ask
  let vEnv = variableEnv env
  let recEnv = recordEnv env
  let entEnv = entityEnv env
  case undefined -- deepProject3
    e of
    Just e' -> either throwError return (typeCheckExpr recEnv entEnv vEnv e' expectedTp)
    Nothing -> expectedExpError

-- |Type check a party expression inside the TM monad.
typeCheckPartyExpr :: ((CoreExp :&: SrcPos) :<: f,
                       (Val :&: SrcPos) :<: f) => Term f -> TM (Either Var Party)
typeCheckPartyExpr e =
    case project e of
      (Just (EVar{expVar = x} :&: (_ :: SrcPos))) ->
          return $ Left x
      _ ->
          case fmap (stripA :: Term (Val :&: SrcPos) -> Term Val) $ deepProject e of
             Just v  -> return $ Right v
             Nothing -> partyExpressionError


--------------------------------------------------------------------------------
-- Type checker error messages
--------------------------------------------------------------------------------

typeError :: String -> TM a
typeError s = do
  pos <- asks srcPos
  throwError $ TypeError s pos

-- |Flatten a list of strings, separated by commas
showList :: [String] -> String
showList = concat . intersperse ", " . map (\x -> "'" ++ x ++ "'")

nonExhaustivePatterns :: [RecordName] -> TM a
nonExhaustivePatterns rNames =
    typeError $ "Non-exhaustive pattern match(es). Patterns not matched: " ++
                showList rNames

clauseDefUndefError :: TemplateName -> TM a
clauseDefUndefError tName =
    typeError $ "Clause template '" ++ tName ++ "' is undefined"

duplicateFieldsError :: [FieldName] -> TM a
duplicateFieldsError dupFields =
    typeError $ "The field(s) " ++ showList dupFields ++
                " are defined more than once"

duplicateVarsError :: [Var] -> TM a
duplicateVarsError dupVars =
    typeError $ "The variable(s) " ++ showList dupVars ++
                " are defined more than once"

undefFieldsError :: [FieldName] -> TransactionType -> TM a
undefFieldsError undefFields eType =
    typeError $ "The field(s) " ++ showList undefFields ++
                " are undefined for transaction type '" ++ eType ++ "'"

templateArgsError :: TemplateName -> Int -> Int -> TM a
templateArgsError tName expCount foundCount =
    typeError $ "Clause template '" ++ tName ++ "' expects " ++
                show expCount ++ " arguments, but " ++
                show foundCount ++ " were found"

templatePartyArgsError :: TemplateName -> Int -> Int -> TM a
templatePartyArgsError tName aexpCount foundCount =
    typeError $ "Clause template '" ++ tName ++ "' expects " ++
                show aexpCount ++ " party arguments, but " ++
                show foundCount ++ " were found"

partyExpressionError :: TM a
partyExpressionError =
    typeError "Only variables and values are allowed in party expressions"

disjunctionPartiesError :: [Either Var Party] -> [Either Var Party] -> TM a
disjunctionPartiesError left right =
    typeError $ "The party occuring in the subexpressions of a disjunction" ++
                " must be identical, but the party(s) of the left disjunct " ++
                "are " ++ showList (map (either show show) left) ++
                " and the party(s) of the right " ++
                "disjunct are " ++ showList (map (either show show) right)

caseCommonSuperTypeError msg =
    typeError $ "Failed to find a common super type for record types used " ++
                "in a case split:\n" ++ msg

templateUndefinedRecord :: TemplateName -> RecordName -> TM a
templateUndefinedRecord tName r =
    typeError $ "Undefined record type '" ++ r ++ "' in template '" ++
                tName ++ "'"

templateDuplicateVarsError :: TemplateName -> [Var] -> TM a
templateDuplicateVarsError tName dupVars =
    typeError $ "The variable(s) " ++ showList dupVars ++
                " are defined more than once for template '" ++ tName ++ "'"

templatePartyMismatch :: TemplateName -> [Var] -> [Either Var Party] -> TM a
templatePartyMismatch tName expectedVars foundVars =
    typeError $ "The party variable(s) " ++ showList expectedVars ++
                " are required to appear in the definition of template '" ++
                tName ++ "', but instead the party(s) " ++
                showList (map (either show show) foundVars) ++ " appeared"

contractDefIllegalType :: TemplateName -> ContractType -> TM a
contractDefIllegalType tName tType =
    typeError $ "The contract template '" ++ tName ++ "' has type '" ++
                tType ++ "', which is not a subtype of '" ++
                contractClass ++ "' as required"

unexptedConstructError :: String -> TM a
unexptedConstructError c = typeError $ "Unexpected construct: " ++ c

expectedExpError :: TM a
expectedExpError = typeError "Expression expected"


--------------------------------------------------------------------------------
-- Exported functions
--------------------------------------------------------------------------------

-- |Type check a contract definition.
typeCheckContractDefinition :: RecEnv -- ^The record typing environment.
                            -> EntityTypingEnv -- ^The entity typing environment.
                            -> VarEnv -- ^Typing environment for prelude functions.
                            -> ContractDefCorePos -- ^The contract definition to type check.
                            -> Either CSLError ()
typeCheckContractDefinition recordEnv entityEnv preludeEnv cDef =
  evalTM env $ typeCheckContractDef cDef
      where env = Environment{variableEnv = preludeEnv,
                              clauseEnv = Map.empty,
                              recordEnv = recordEnv,
                              entityEnv = entityEnv,
                              srcPos = Nothing}

instance (TypeC f h, TypeC g h) => TypeC (f :+: g) h where
    typeC (Inl x) = typeC x
    typeC (Inr x) = typeC x
