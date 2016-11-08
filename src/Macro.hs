{-# LANGUAGE TupleSections #-}

module Macro
  ( expandMacro
  , defineSyntax
  ) where

import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Data.Foldable (foldrM)

import Definition
import LispVector (vectorLength, vectorToList)
import Variable


expandMacro :: LispVal -> Env -> IOEvaled LispVal
expandMacro lispval@(LList (LAtom var:vals)) env =
    liftIO (isBound var env) >>= \bound ->
      if bound
        then getVar var env >>= expandIfMacro (LList vals)
        else return lispval
  where
    expandIfMacro :: LispVal -> LispVal -> IOEvaled LispVal
    expandIfMacro inForm (LSyntax (SyntaxDef _ literals rules)) =
        runMaybeT (matchFirst inForm rules) >>=
        maybe (throwError $ invalidSyntax lispval) return
      where
        matchFirst :: LispVal -> [SyntaxRule] -> MaybeT IOEvaled LispVal
        matchFirst val (r:rs) = matchPattern val env literals r `mplus` matchFirst val rs
        matchFirst _   _      = mzero

    expandIfMacro _ _ = return lispval
expandMacro val _ = return val

invalidSyntax :: LispVal -> LispError
invalidSyntax = BadSpecialForm "Invalid syntax"

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

matchPattern :: LispVal -> Env -> [String] -> SyntaxRule -> MaybeT IOEvaled LispVal
matchPattern val env literals (pat, template) = do
    p                 <- liftMaybe $ dropSynId pat
    bindings          <- liftMaybe $ matchPat (val, p)
    (newEnv, renames) <- liftIO $ bindMacroEnv bindings
    lift $ transformTemplate newEnv renames template

  where
    bindMacroEnv :: [(VarName, LispVal)] -> IO (Env, [(VarName, VarName)])
    bindMacroEnv = foldrM (\(name, val) (env, renames) -> do
      bound <- isBound name env
      if bound
        then let newname = '0':name
             in (, (name, newname):renames) <$> (env `bindVars` [(newname, val)])
        else (, renames) <$> (env `bindVars` [(name, val)]))
      (env, [])

    {-
     - Note:
     - * '_' currently can be mapped as a binding to be referenced inside
     -   pattern templates
     -
     - TODO: Support '...' ellipsis in patterns
     -}
    matchPat :: (LispVal, LispVal) -> Maybe [(VarName, LispVal)]
    matchPat (inF, LAtom lit)
        -- pattern expects syntax literal
      | lit `elem` literals = case inF of
          LAtom a | a == lit  -> return []
                  | otherwise -> Nothing
          _ -> Nothing
        -- bind pattern as binding for value in environment
      | otherwise           = return [(lit, inF)]

    matchPat (LList vs,         LList ps)
      | length vs /= length ps = Nothing
      | otherwise              = fmap join . traverse matchPat $ zip vs ps

    matchPat (LList vs,         LDottedList ps p)
      | length vs < length ps = Nothing
      | otherwise             =
          let (vs', vals) = splitAt (length ps) vs
          in  (++) <$> matchPat (LList vals, p)
                   <*> fmap join (traverse matchPat $ zip vs' ps)

    matchPat (LDottedList vs v, LDottedList ps p)
      | length vs /= length ps = Nothing
      | otherwise              = (++) <$> matchPat (v, p)
                                      <*> fmap join (traverse matchPat $ zip vs ps)

    matchPat (LVector vec,      LVector pVec)
      | vectorLength vec /= vectorLength pVec = Nothing
      | otherwise                             =
            fmap join . traverse matchPat
          $ zip (vectorToList vec) (vectorToList pVec)

    matchPat _ = Nothing

    dropSynId :: LispVal -> Maybe LispVal
    dropSynId (LList (_:ps))         = Just $ LList ps
    dropSynId (LDottedList (_:ps) p) = Just $ LDottedList ps p
    dropSynId _                      = Nothing

    transformTemplate :: Env -> [(VarName, VarName)] -> LispVal -> IOEvaled LispVal
    transformTemplate env renames var@(LAtom lit) = case lookup lit renames of
      Just newname -> getVar newname env
      _            -> do
        bound <- liftIO $ isBound lit env
        if bound
          then getVar lit env
          else return var

    transformTemplate env renames (LList vals) =
      LList <$> traverse (transformTemplate env renames) vals

    transformTemplate env renames (LDottedList vals val) =
      LDottedList <$> traverse (transformTemplate env renames) vals
                  <*> transformTemplate env renames val

    transformTemplate env renames (LVector vec) =
      LVector <$> traverse (transformTemplate env renames) vec

    transformTemplate _ _ template = return template

liftMaybe :: (Monad m) => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return
