module Macro
  ( expandMacro
  , defineSyntax
  ) where

import Control.Monad.Except
import Data.Maybe (fromMaybe)

import Definition
import LispVector (vectorLength, vectorToList)
import Variable

expandMacro :: LispVal -> Env -> IOEvaled LispVal
expandMacro lispval env = case lispval of
    -- TODO: Support macro invocation if input form is of improper dotted list
    LList (LAtom var:vals) -> liftIO (isBound var env) >>=
      \bound -> if bound
                  then getVar var env >>= expandIfMacro (LList vals)
                  else return lispval
  where
    expandIfMacro :: LispVal -> LispVal -> IOEvaled LispVal
    expandIfMacro inForm (LSyntax (SyntaxDef closure literals rules)) =
        maybe (throwError invalidSyntaxErr)
              return
              (matchFirst inForm rules)
      where matchFirst :: LispVal -> [SyntaxRule] -> Maybe LispVal
            matchFirst val (r:rs) = matchPattern val closure literals r `mplus` matchFirst val rs
            matchFirst _   _      = Nothing
    expandIfMacro _ _ = return lispval

    invalidSyntaxErr :: LispError
    invalidSyntaxErr = BadSpecialForm "Invalid syntax" lispval

defineSyntax :: [LispVal] -> Env -> IOEvaled LispVal
defineSyntax [ synId@(LAtom identifier), synRs@(LList xs) ] env = case xs of
  (LAtom "syntax-rules" : LList literals : rules) -> do
      ls <- mapM validateLiteral literals
      rs <- mapM validateRule rules
      let syntax = LSyntax $ SyntaxDef env ls rs
      liftIO $ defineVar identifier syntax env
      return synId

  _ -> throwError $ BadSpecialForm "Invalid 'syntax-rules'" synRs
defineSyntax args _ = throwError . BadSpecialForm "Invalid 'define-syntax' " $ LList args

validateLiteral :: LispVal -> IOEvaled String
validateLiteral (LAtom lit) = return lit
validateLiteral val         = throwError $ BadSpecialForm "Invalid 'syntax-rules'" val

validateRule :: LispVal -> IOEvaled SyntaxRule
validateRule val@(LList [pat, expr]) = case pat of
    LList xs         -> if null xs
                          then throwError badSyntaxErr
                          else return (pat, expr)
    LDottedList xs e -> if null xs
                          then throwError badSyntaxErr
                          else return (pat, expr)
    _                -> throwError badSyntaxErr
  where badSyntaxErr :: LispError
        badSyntaxErr = BadSpecialForm "Invalid 'syntax-rules'" val
validateRule val = throwError $ BadSpecialForm "Invalid 'syntax-rules'" val

matchPattern :: LispVal -> Env -> [String] -> SyntaxRule -> Maybe LispVal
matchPattern val env literals (pat, expr) = do
    p <- dropSynId pat
    if matchPat (val, p)
      then transformSyntax
      else Nothing
  where
    {-
     - TODO: Combine 'matchPat' and 'transformSyntax' into one function?
     -       - perhaps compute a maybe action that can be 'sequenced' to
     -         execute the transform syntax functionality
     -}

    -- Note: matchPat :: (InputFormLispVal, PatternLispVal) -> Bool
    matchPat :: (LispVal, LispVal) -> Bool
    matchPat (inF, LAtom lit)
        -- pattern expects syntax literal
      | lit `elem` literals = case inF of
          LAtom a -> a == lit
          _       -> False
        -- bind pattern as binding for value in environment
      | otherwise           = True

    matchPat (LList vs,         LList ps)
      | length vs /= length ps = False
      | otherwise              = all matchPat $ zip vs ps
    matchPat (LList vs,         LDottedList ps p)
      | length vs < length ps = False
      | otherwise             = all matchPat $ zip vs ps
    matchPat (LDottedList vs v, LDottedList ps p)
      | length vs /= length ps = False
      | otherwise              = matchPat (v, p) && all matchPat (zip vs ps)
    matchPat (LVector vec,      LVector pVec)
      | vectorLength vec /= vectorLength pVec = False
      | otherwise                             = all matchPat $ zip (vectorToList vec) (vectorToList pVec)
    matchPat _ = False

    transformSyntax :: Maybe LispVal
    transformSyntax = return $ LString "macro matched" -- TODO: Temp placeholder value

    dropSynId :: LispVal -> Maybe LispVal
    dropSynId (LList (_:ps))         = Just $ LList ps
    dropSynId (LDottedList (_:ps) p) = Just $ LDottedList ps p
    dropSynId _                      = Nothing
