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


-- Note: Syntax Id is stripped away from inForm before passing to matchPattern
expandMacro :: LispVal -> Env -> IOEvaled LispVal
expandMacro lispval env = case lispval of
    LList (LAtom mId:vals)           -> expandIfMacro mId $ LList vals
    LDottedList (LAtom mId:vals) val -> expandIfMacro mId $ LDottedList vals val
    _                                -> return lispval
  where
    expandIfMacro :: VarName -> LispVal -> IOEvaled LispVal
    expandIfMacro var val = liftIO (isBound var env) >>= \bound ->
      if bound
        then getVar var env >>= expand val
        else return lispval

    expand :: LispVal -> LispVal -> IOEvaled LispVal
    expand inForm (LSyntax (SyntaxDef _ literals rules)) =
        runMaybeT (matchFirst inForm rules) >>=
        maybe (throwError $ invalidSyntax lispval) return
      where
        matchFirst :: LispVal -> [SyntaxRule] -> MaybeT IOEvaled LispVal
        matchFirst val (r:rs) = matchPattern val env literals r `mplus` matchFirst val rs
        matchFirst _   _      = mzero

    expand _ _ = return lispval

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

type InputLispVal = LispVal

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

    matchPat' (LDottedList pats pat) = get >>= \inF -> case (inF, pats) of
      ------- "InF" matches 'LList' -------

      -- length of input list < head length of pat dotted list
      (LList []        , _:_) -> lift Nothing

      -- length of input list > head length of pat dotted list
      (LList _         , [] ) -> matchPat' pat

      -- input list `match` head of pat dotted list
      (LList (v:vs), p:ps) -> do
        bindings <- lift $ evalStateT (matchPat' p) v
        put $ LList vs
        mappend bindings <$> matchPat' (LDottedList ps pat)


      ------- "InF" matches 'LDottedList' -------

      -- head length of input dotted list == head length of pat dotted list
      (LDottedList [] v, [] ) -> put v >> matchPat' pat

      -- head of input dotted list `match` ellipsis in head of pat dotted list
      (LDottedList _ _, p:LAtom "...":ps) -> do
        bindings <- matchEllipsis p
        mappend bindings <$> matchPat' (LDottedList ps pat)

      -- head of input dotted list `match` head of pat dotted list
      (LDottedList (v:vs) val, p:ps) -> do
        bindings <- lift $ evalStateT (matchPat' p) v
        put $ LDottedList vs val
        mappend bindings <$> matchPat' (LDottedList ps pat)

      _ -> lift Nothing

    -- TODO: Add cases for any other type of pattern lispval
    --       - are any other patterns valid?
    matchPat' _ = lift Nothing

    matchEllipsis :: PatLispVal -> StateT InputLispVal Maybe Bindings
    -- TODO: Consider moving 'validateInputVal' to the end of match
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
            _            -> return $ matchEllipsisEmpty pat

          LList _ -> return $ matchEllipsisEmpty pat

          LDottedList vs val -> case runStateT matchE $ LList vs of
            Just (binding, LList vs') -> put (LDottedList vs' val)
                                      >> return binding
            _ -> return $ matchEllipsisEmpty pat

    consBindings :: LispVal -> LispVal -> LispVal
    consBindings (LList [LAtom ".", v]) (LList (LAtom ".":vs)) = LList $ LAtom ".":v:vs

    dropSynId :: LispVal -> Maybe LispVal
    dropSynId (LList (_:ps))         = Just $ LList ps
    dropSynId (LDottedList (_:ps) p) = Just $ LDottedList ps p
    dropSynId _                      = Nothing

-- TODO: Implement matching ellipsis pattern with empty input vals
--       - support dotted list ellipsis patterns and cover remaining cases
--         ie: undefined case valid?
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
     - TODO: Reverse template then call transformTemplate?
     -       - reverse template if list so that '...' is traversed before
     -         the actual identifier
     -       - after recursion, fmap over the results to 'spread/flatten' the list
     -       - this may ease expansion of nestsed ellipsis
     -       - also can simplify matching process from "temp/macrotest.scm -  ex3, ex4"
     -       - Consider 'paramorphism' recursion scheme
               - in the ex4 case, add pattern contains ellipsis flag
     -}
    lift $ evalStateT (transformTemplate' senv) template

  where
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
