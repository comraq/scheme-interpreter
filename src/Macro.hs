{-# LANGUAGE TupleSections #-}

module Macro
  ( expandMacro
  , defineSyntax
  ) where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Foldable (foldMap)
import qualified Data.Map as M

import Definition
import LispVector (vectorLength, vectorToList, vector)
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

type PatLispVal      = LispVal
type InputLispVal    = LispVal
type TemplateLispVal = LispVal

type Renames   = [(VarName, VarName)]
type Bindings  = M.Map VarName LispVal
type SyntaxEnv = (Bindings, Renames)

makeSyntaxEnv :: LispVal -> Env -> [String] -> SyntaxRule -> MaybeT IO SyntaxEnv
makeSyntaxEnv val env literals (pat, template) = do
    pat'     <- liftMaybe $ dropSynId pat
    bindings <- liftMaybe $ evalStateT (matchPat' pat') val
    liftIO $ bindMacroEnv' env bindings

  where
    -- Note:
    -- * '_' currently can be mapped as a binding to be referenced inside
    --   pattern templates
    matchPat' :: PatLispVal -> StateT InputLispVal Maybe Bindings
    matchPat' (LAtom lit) = get >>= \inF ->
      lift $ if lit `elem` literals
               then case inF of
                      LAtom a | a == lit  -> Just mempty
                              | otherwise -> Nothing
                      _ -> Nothing
               else Just $ M.singleton lit inF

    matchPat' (LList pats) = get >>= \inF -> case (inF, pats) of
      (LList [], [])        -> return mempty
      (_, p:LAtom "...":ps) -> do
        put inF
        bindings <- matchEllipsis p
        mappend bindings <$> matchPat' (LList ps)

      (LList (v:vs), p:ps)  -> do
        bindings <- lift $ evalStateT (matchPat' p) v
        put $ LList vs
        mappend bindings <$> matchPat' (LList ps)

      _ -> lift Nothing

    matchPat' (LVector pVec) = get >>= \inF -> case inF of
      LVector vec ->
        let vs = vectorToList vec
            ps = vectorToList pVec
        in  put (LList vs) >> matchPat' (LList ps)
      _           -> lift Nothing

    -- TODO: Add cases for dotted lists where input is dotted list or input
    --       is list with remaining vals captured in the last pat of dotted list
    matchPat' _ = undefined

    matchEllipsis :: PatLispVal -> StateT InputLispVal Maybe Bindings
    -- TODO: Move 'validateInputVal' to the end of match
    --       ie: if bindings is empty list, return Nothing
    matchEllipsis pat = get >>= validateInputVal >> matchE
      where
        validateInputVal :: InputLispVal -> StateT InputLispVal Maybe ()
        validateInputVal val@(LList _)         = return ()
        validateInputVal val@(LDottedList _ _) = return ()
        validateInputVal _                     = lift Nothing

        matchE :: StateT InputLispVal Maybe Bindings
        matchE = get >>= \inF -> case inF of
          LList (v:vs) -> case evalStateT (matchPat' pat) v of
            Just binding -> do
              put $ LList vs
              bindings <- matchE
              let eBinding = ellipsis <$> binding
              return $ M.unionWith consBindings eBinding bindings
            -- TODO: should be 'return $ matchEllipsisEmpty pat'?
            _            -> return M.empty

          LList _ -> return $ matchEllipsisEmpty pat

          -- TODO: Support Ellipsis match with dotted list patterns
          LDottedList invs lastInv -> undefined
          _ -> undefined

    consBindings :: LispVal -> LispVal -> LispVal
    consBindings (LList [LAtom ".", v]) (LList (LAtom ".":vs)) = LList $ LAtom ".":v:vs

    dropSynId :: LispVal -> Maybe LispVal
    dropSynId (LList (_:ps))         = Just $ LList ps
    dropSynId (LDottedList (_:ps) p) = Just $ LDottedList ps p
    dropSynId _                      = Nothing

-- TODO: Implement matching ellipsis pattern with empty input vals
--       - support dotted list ellipsis patterns and cover remaining cases
matchEllipsisEmpty :: PatLispVal -> Bindings
matchEllipsisEmpty (LAtom p)  = M.singleton p emptyEllipsis
matchEllipsisEmpty (LList ps) = foldMap matchEllipsisEmpty ps
matchEllipsisEmpty _          = undefined

bindMacroEnv' :: Env -> Bindings -> IO SyntaxEnv
bindMacroEnv' env bindings = do
  varBindings <- getBindings env
  return $ M.foldrWithKey renameIfBound (varBindings, []) bindings

renameIfBound :: String -> v -> (M.Map String v, [(String, String)]) -> (M.Map String v, [(String, String)])
renameIfBound name v (m, renames) =
  if M.member name m
    then let newname = '0':name
         in  (M.insert newname v m, (name, newname):renames)
    else (M.insert name v m, renames)


matchPattern :: LispVal -> Env -> [String] -> SyntaxRule -> MaybeT IOEvaled LispVal
matchPattern val env literals (pat, template) = do
    let maybeSyntaxEnv = liftIO . runMaybeT $ makeSyntaxEnv val env literals (pat, template)
    senv <- MaybeT maybeSyntaxEnv
    {-
     - TODO: Reverse template then call tranformTemplate?
     -       - reverse template if list so that '...' is traversed before
     -         the actual identifier
     -       - after recursion, fmap over the results to 'spread/flatten' the list
     -}
    lift $ evalStateT (transformTemplate' senv) template

  where
    -- TODO: Iterate over input form to expand ellipsis if necessary
    transformTemplate' :: SyntaxEnv -> StateT TemplateLispVal IOEvaled LispVal
    transformTemplate' (bindings, renames) = go
      where
        go :: StateT TemplateLispVal IOEvaled LispVal
        go = get >>= \inF -> case inF of

          LAtom lit -> case lookup lit renames of
            Just newname -> return $ bindings M.! newname
            _            -> if M.member lit bindings
                              then return $ bindings M.! lit
                              else return inF

          -- TODO: Ellipsis can match any LispVal, with ellipsis
          --       identifier nested in lists/dotted lists/vectors
          --       - support nested ellipsis val expansion
          LList (val:LAtom "...":vals) -> do
            v <- lift $ evalStateT go val
            case maybeEllipsisVal v of
              Just []  -> put (LList vals) >> go
              Just evs -> do
                put $ LList vals
                (\(LList vs) -> LList $ evs ++ vs) <$> go
              _        -> throwError $ BadSpecialForm "Missing ellipsis after identifier" val

          LList (val:vals) -> do
            v  <- lift $ evalStateT go val
            put $ LList vals
            (\(LList vs) -> LList $ v:vs) <$> go

          _ -> return inF

  {-
    bindMacroEnv :: [(VarName, LispVal)] -> IO (Env, [(VarName, VarName)])
    bindMacroEnv = foldrM (\(name, val) (env, renames) -> do
      bound <- isBound name env
      if bound
        then let newname = '0':name
             in (, (name, newname):renames) <$> (env `bindVars` [(newname, val)])
        else (, renames) <$> (env `bindVars` [(name, val)]))
      (env, [])

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

    transformTemplate :: SyntaxEnv -> TemplateLispVal -> IOEvaled LispVal
    transformTemplate (env, renames, ellipsis) var@(LAtom lit) = case lookup lit renames of
      Just newname -> getVar newname env
      _            -> do
        bound <- liftIO $ isBound lit env
        if bound
          then getVar lit env
          else return var

    transformTemplate senv (LList vals) =
      LList <$> traverse (transformTemplate senv) vals

    transformTemplate senv (LDottedList vals val) =
      LDottedList <$> traverse (transformTemplate senv) vals
                  <*> transformTemplate senv val

    transformTemplate senv (LVector vec) =
      LVector <$> traverse (transformTemplate senv) vec

    transformTemplate _ template = return template
    -}

liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return


------- Helper Functions for Ellipsis Values -------

ellipsis :: LispVal -> LispVal
ellipsis v = LList [LAtom ".", v]

emptyEllipsis :: LispVal
emptyEllipsis = LList [LAtom "."]

maybeEllipsisVal :: LispVal -> Maybe [LispVal]
maybeEllipsisVal (LList (LAtom ".":vs)) = Just vs
maybeEllipsisVal _                      = Nothing
