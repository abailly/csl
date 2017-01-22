{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Contracts.Language.CSL.Render
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- Rendering of CSL expressions, clauses, clause/contract definitions, and
-- closures.
--
--------------------------------------------------------------------------------

module Poets.Contracts.Language.CSL.Render
    (
     Doc,
     contractDefToDoc,
     clauseDefToDoc,
     functionDefToDoc,
     closureToDoc,
     clauseToDoc
    ) where

import           Data.List
import qualified Data.Map                         as Map
import           Data.Maybe
import           Poets.Contracts.Language.CSL.AST
import           Poets.Data                       hiding (empty)
import           Poets.Data.Render
import           Prelude                          hiding ((<$>))
import           Text.PrettyPrint.Leijen          hiding (pretty)

-- |Pretty printing of contract definitions.
contractDefToDoc :: (Functor c, Functor e, Render e, Render c)
                    => ContractDef e c -> Doc
contractDefToDoc ContractDefinition{contractDefName = tName,
                                    contractDefType = tp,
                                    contractDefClauseDefs = cDefs,
                                    contractDefFunDefs = fDefs,
                                    contractDefBody = c} =
    text "name" <> colon <+> text tName <$>
    text "type" <> colon <+> text tp <$>
    empty <$>
    vsep (map functionDefToDoc fDefs) <$>
    empty <$>
    vsep (map clauseDefToDoc $ Map.elems cDefs) <$>
    empty <$>
    text "contract" <+> equals <$> renderTerm c

-- |Pretty printing of clause definitions.
clauseDefToDoc :: (Functor c, Render c) => ClauseDef c -> Doc
clauseDefToDoc ClauseDefinition{clauseDefName = tName,
                                clauseDefParams = params,
                                clauseDefPartyParams = partyParams,
                                clauseDefBody = c} =
    text "clause" <+>
    text tName <>
    tupled (map (\(x,tp) -> text x <+> colon <+> text (show tp)) params) <>
    encloseSep langle rangle comma
               (map (\(x,tp) -> text x <+> colon <+> text (show tp))
                    partyParams) <+>
    equals <$>
    indent 1 (renderTerm c)

functionDefToDoc :: (Render e, Functor e) => FunctionDef e -> Doc
functionDefToDoc FunctionDefinition{functionName = fName,
                                           functionExp = e} =
    text "val" <+> text fName <+> equals <+> renderTerm e


--------------------------------------------------------------------------------
-- Clauses and closures
--------------------------------------------------------------------------------

instance Render CoreClause where
    render Fulfilment{} =
        text "fulfilment"
    render Obligation{clauseResponsible = r,
                      clauseTransactionType = trType,
                      clauseBinders = b,
                      clauseRemainderVar = x,
                      clausePredicate = p,
                      clauseDeadline = d,
                      clauseContinuation = c} =
        langle <> r <> rangle <+>
        align (text trType <>
               (if not $ null b then
                    tupled (map (\(x,f) -> text f <+> text x) b)
                else
                    empty) <$>
               predToDoc p <$>
               deadlineToDoc d <$>
               text "remaining" <+> text x) <$>
        text "then" <$> c
    render ExternalChoice{clauseTransactionType = trType,
                          clauseBinders = b,
                          clauseRemainderVar = x,
                          clausePredicate = p,
                          clauseDeadline = d,
                          clauseContinuation = c1,
                          clauseElse = c2} =
        text "when" <+>
        align (text trType <>
               (if not $ null b then
                    tupled (map (\(x,f) -> text f <+> text x) b)
                else
                    empty) <$>
               predToDoc p <$>
               deadlineToDoc d <$>
               text "remaining" <+> text x) <$>
        text "then" <$>
        indent 1 c1 <$>
        text "else" <$>
        indent 1 c2
    render InternalChoice{clauseCondition = e,
                          clauseThen = c1,
                          clauseElse = c2} =
        text "if" <+> e <+> text "then" <$>
        indent 1 c1 <$>
        text "else" <$>
        indent 1 c2
    render And{clauseLeft = c1, clauseRight = c2} =
        align (parens c1) <$>
        text "and" <$>
        align (parens c2)
    render Or{clauseLeft = c1, clauseRight = c2} =
        align (parens c1) <$>
        text "or" <$>
        align (parens c2)
    render Case{clauseCaseExpr = e, clauseCases = cs} =
        text "case" <+> e <+> text "of" <$>
        indent 1 (vsep $ intersperse (text "|") $
                       map (\c -> text (clauseCaseRecordName c) <+>
                                  text (clauseCaseVar c) <+>
                                  text "->" <+>
                                  clauseCaseBody c) cs)
    render Instantiate{clauseClauseTemplate = tName,
                       clauseArgs = args,
                       clausePartyArgs = partyArgs} =
        text tName <>
        tupled args <>
        encloseSep langle rangle comma partyArgs

instance Render SugClause where
    render ObligationSug{clauseSugResponsible = r,
                         clauseSugTransactionType = trType,
                         clauseSugBinders = b,
                         clauseSugRemainderVar = x,
                         clauseSugPredicate = p,
                         clauseSugDeadline = d,
                         clauseSugContinuation = c} =
        langle <> r <> rangle <+>
        align (text trType <>
               (if not $ null b then
                    tupled (map (\(x,f) -> text f <+> text x) b)
                else
                    empty) <$>
               maybe empty (\x -> predToDoc x <$> empty) p <>
               deadlineSugToDoc d <>
               maybe empty (\x -> empty <$> text "remaining" <+> text x) x) <>
        maybe empty (\c -> empty <$> text "then" <$> c) c
    render ExternalChoiceSug{clauseSugTransactionType = trType,
                             clauseSugBinders = b,
                             clauseSugRemainderVar = x,
                             clauseSugPredicate = p,
                             clauseSugDeadline = d,
                             clauseSugContinuation = c1,
                             clauseSugElse = c2} =
        text "when" <+>
        align (text trType <>
               (if not $ null b then
                    tupled (map (\(x,f) -> text f <+> text x) b)
                else
                    empty) <$>
               maybe empty (\x -> predToDoc x <$> empty) p <>
               maybe empty (\x -> predToDoc x <$> empty) p <>
               deadlineSugToDoc d <>
               maybe empty (\x -> empty <$> text "remaining" <+> text x) x) <>
        maybe empty (\c -> empty <$> text "then" <$> indent 1 c) c1 <>
        maybe empty (\c -> empty <$> text "else" <$> indent 1 c) c2

-- |Pretty printing of predicates.
predToDoc :: Doc -> Doc
predToDoc p = text "where" <+> p

-- |Pretty printing of deadlines.
deadlineToDoc :: Deadline Doc -> Doc
deadlineToDoc Deadline{deadlineWithin = e1, deadlineAfter = e2} =
    text "due" <+> text "within" <+> e1 <+> text "after" <+> e2

-- |Pretty printing of deadlines with syntactic sugar.
deadlineSugToDoc :: DeadlineSug Doc -> Doc
deadlineSugToDoc Immediately{deadlineSugAfterM = Nothing} =
    text "immediately"
deadlineSugToDoc Immediately{deadlineSugAfterM = Just e} =
    text "immediately after" <+> e
deadlineSugToDoc Within{deadlineSugWithin = e,
                        deadlineSugAfterM = Nothing} =
    text "due within" <+> e
deadlineSugToDoc Within{deadlineSugWithin = e1,
                        deadlineSugAfterM = Just e2} =
    text "due within" <+> e1 <+> text "after" <+> e2
deadlineSugToDoc After{deadlineSugAfter = e} =
    text "due after" <+> e

-- |Pretty printing of CSL closures, that is, running CSL contracts.
closureToDoc :: DateTime -> Closure -> Doc
closureToDoc dt Closure{closureClause = c,
                        closureClauseDefs = cDefs,
                        closureFunctionDefs = fDefs} =
    vsep (map (functionDefToDoc . snd) $ Map.toList fDefs) <$>
    empty <$>
    vsep (map (clauseDefToDoc . snd) $ Map.toList cDefs) <$>
    empty <$>
    text "contract" <+> equals <$>
    indent 1 (renderTerm c <$>
              text "starting" <+> render (VDateTime dt))

-- |Pretty printing of a clause.
clauseToDoc :: ClauseCore -> Doc
clauseToDoc = renderTerm


--------------------------------------------------------------------------------
-- Expressions and types
--------------------------------------------------------------------------------

instance Render Val where
    render (VInt n) =
        int n
    render (VBool True) =
        text "true"
    render (VBool False) =
        text "false"
    render (VString s) =
        dquotes $ text s
    render (VDate d) =
        let (year, month, day) = dateToYearMonthDay d in
        text $ "<<" ++ show year ++ "-" ++ show month ++ "-" ++ show day ++ ">>"
    render (VTime t) =
        let (hour, minute, second, microsecond) = timeToHourMinSecMicroSec t in
        text $ "<<" ++ show hour ++ ":" ++ show minute ++ ":" ++ show second ++
               (if microsecond == 0 then "" else '.' : show microsecond) ++ ">>"
    render (VDateTime dt) =
        let (date, time) = dateTimeToDateTime dt in
        let (year, month, day) = dateToYearMonthDay date in
        let (hour, minute, second, microsecond) = timeToHourMinSecMicroSec time in
        text $ "<<" ++ show year ++ "-" ++ show month ++ "-" ++ show day ++
               " " ++ show hour ++ ":" ++ show minute ++ ":" ++ show second ++
               (if microsecond == 0 then "" else '.' : show microsecond) ++ ">>"
    render (VDuration vd) =
        let vd' = normaliseDuration vd in
        let durs = [printDur (durationSeconds vd') 'S',
                    printDur (durationMinutes vd') 'm',
                    printDur (durationHours vd') 'H',
                    printDur (durationDays vd') 'D',
                    printDur (durationWeeks vd') 'W',
                    printDur (durationMonths vd') 'M',
                    printDur (durationYears vd') 'Y'] in
        let nonNulls = intersperse (text "<+>") $ catMaybes durs in
        if null nonNulls then
            int 0 <> char 'S'
        else
            hsep nonNulls
            where printDur :: Int -> Char -> Maybe Doc
                  printDur 0 _ = Nothing
                  printDur n c = Just (int n <> char c)
    render (VReal d) =
        double d
    render (VEnt VEntity{ventType = rName, ventId = id, ventContext = mDt}) =
        text rName <>
        angles (int id <> maybe empty (\dt -> comma <> text (show dt)) mDt)
    render (VRecord VR{vrecordName = rName, vrecordFields = fields}) =
        text rName <>
        if fieldsNull fields then
            empty
        else
            encloseSep lbrace rbrace comma $ map renderVField $
                                                 fieldsList fields
            where renderVField vf = text (vfieldName vf) <+>
                                    equals <+>
                                    vfieldValue vf
    render (VList vs) =
        encloseSep lbracket rbracket comma vs

instance Render VUnit where
    render VUnit = text "()"

instance Render VFun where
    render (VFun _) = text "<function>"

instance Render CoreExp where
    render EVar{expVar = x} =
        text x
    render ELambda{expLambdaParam = var, expLambdaBody = e} =
        parens $ text "\\" <> text var <+> text "->" <+> e
    render EApply{expApplyFn = e1, expApplyArg = e2} =
        parens $ e1 <+> e2
    render EProj{expProjFieldName = f, expProjRecord = e} =
        parens $ e <> dot <> text f
    render EUpdate{expUpdateRecord = e1,
                   expUpdateFieldName = f,
                   expUpdateValue = e2} =
        parens $ e1 <> braces (text f <+> equals <+> e2)
    render EBinOp{expBinOp = o, expBinOpArg1 = e1, expBinOpArg2 = e2} =
        parens $ e1 <+> text (show o) <+> e2
    render EIfThenElse{expCondition = c,
                       expConditionThen = e1,
                       expConditionElse = e2} =
        text "if" <+> c <+> text "then" <+> e1 <+> text "else" <+> e2
    render ECase{expCase = e, expCaseType = tp, expCases = cs} =
        text "case" <+> e <+>
        maybe empty (\r -> colon <+> text r <+> empty) tp <>
        text "of" <$>
        indent 1 (vsep $ intersperse (text "|") $
                         map (\CaseExp{caseExpRecordName = rName,
                                       caseExpVar = x,
                                       caseExpBody = e} ->
                              text rName <+> text x <+> text "->" <+> e) cs)

instance Render SugExp where
    render ESUnOp{expSugUnOp = o, expSugUnOpArg = e} =
        parens $ text (show o) <> e
    render ESBinOp{expSugBinOp = o,
                   expSugBinOpArg1 = e1,
                   expSugBinOpArg2 = e2} =
        parens $ e1 <+> text (show o) <+> e2
    render ESLet{expSugLetName = x, expSugLetDef = e1, expSugLetBody = e2} =
        parens $ text "let" <+> text x <+> equals <+> e1 <+> text "in" <+> e2
    render ESLambda{expSugLambdaParams = params, expSugLambdaBody = body} =
        parens $ text "\\" <> hsep (map text params) <+> text "->" <+> body
    render ESUpdate{expSugUpdateRecord = x, expSugUpdateList = l} =
        parens $ x <> encloseSep lbrace rbrace comma (map (\(f,e) -> text f <+> equals <+> e) l)

instance Render TypeUnit where
    render TUnit = text "()"

instance Render TypeVar where
    render (TVar a) = text a

instance Render TypeFunction where
    render (TFunction tp1 tp2) =
        parens $ tp1 <+> text "->" <+> tp2
