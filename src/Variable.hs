{-# LANGUAGE TupleSections
           , FlexibleContexts
  #-}

module Variable
  ( runIOEvaled
  , runIOEvaledSafe
  , emptyEnv
  , liftEvaled
  , trapError
  , runEvaled
  , bindingNotFound

  , getVar
  , getVarDeep

  , setVar
  , defineVar
  , bindVars

  , readPtrVal
  , modifyPtrVal
  , toPtrVal

  , isBound
  ) where

import Control.Monad.Except
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import Definition


{-
 - Signatures of the commonly used 'IORef' functions below:
 -   newIORef   :: a            -> IO (IORef a)
 -   readIORef  :: IORef a      -> IO a
 -   writeIORef :: IORef a -> a -> IO ()
 -}
emptyEnv :: IO Env
emptyEnv = newIORef M.empty

liftEvaled :: Evaled a -> IOEvaled a
liftEvaled = either throwError return . runExcept

runEvaled :: (LispError -> b) -> (a -> b) -> Evaled a -> b
runEvaled errHandler sucHandler = either errHandler sucHandler . runExcept

runIOEvaled :: (LispError -> b) -> (a -> b) -> IOEvaled a -> IO b
runIOEvaled errHandler sucHandler = fmap (either errHandler sucHandler)
                                  . runExceptT

runIOEvaledSafe :: IOEvaled String -> IO String
runIOEvaledSafe = runIOEvaled undefined id . trapError

-- Check if a variable has a binding in the environment
isBound :: Env -> VarName -> IO Bool
isBound env var = M.member var <$> readIORef env

readPtrVal :: LispVal -> IOEvaled LispVal
readPtrVal (LPointer ptrVal) = liftIO $ readIORef ptrVal
readPtrVal val               = throwError $ ImmutableArg "Not pointer value" val

{-
 - Gets the variable from the environment
 - - read environment into 'IO' monad, then lift into the current monad
 -   transformer stack
 - - lookup the variable in the environment:
 -   - if found, read from 'IORef' to 'IO' then lift back into the current
 -     stack
 -   - 'UnboundVar' error otherwise
 -}
getVar :: Env -> VarName -> IOEvaled LispVal
getVar env var = do
    vars <- liftIO $ readIORef env
    fromMap notFoundErr var vars
  where notFoundErr :: LispError
        notFoundErr = bindingNotFound var

getVarDeep :: Env -> VarName -> IOEvaled LispVal
getVarDeep env var = do
    vars    <- liftIO $ readIORef env
    lispVal <- fromMap notFoundErr var vars
    case lispVal of
      LPointer ptr -> liftIO $ readIORef ptr
      _            -> return lispVal

  where notFoundErr :: LispError
        notFoundErr = bindingNotFound var

setVar :: Env -> VarName -> LispVal -> IOEvaled LispVal
setVar env var value = do
    defined <- liftIO $ isBound env var
    if defined
      then liftIO $ env `modifyIORef` M.insert var value
      else throwError undefinedErr

    -- Returns the set value for convenience
    return value

  where undefinedErr :: LispError
        undefinedErr = UnboundVar "Setting an unbound variable" var

modifyPtrVal :: (LispVal -> IOEvaled LispVal) -> LispVal -> IOEvaled LispVal
modifyPtrVal f (LPointer ptr) = do
  oldVal <- liftIO $ readIORef ptr
  newVal <- f oldVal
  liftIO $ writeIORef ptr newVal
  return newVal
modifyPtrVal _ val = throwError $ ImmutableArg "Modifying non-mutable value" val

defineVar :: Env -> VarName -> LispVal -> IOEvaled LispVal
defineVar env var value =
  liftIO $ env `modifyIORef` M.insert var value >> return value

bindVars :: Env -> [(VarName, LispVal)] -> IO Env
bindVars env bindings = modifyIORef env extendEnv >> return env
  where
    extendEnv :: M.Map VarName LispVal -> M.Map VarName LispVal
    extendEnv envMap = foldr addToMap envMap bindings

    addToMap :: Ord k => (k, v) -> M.Map k v -> M.Map k v
    addToMap = uncurry M.insert

toPtrVal :: LispVal -> IO LispVal
toPtrVal = fmap LPointer . newIORef

trapError :: MonadError LispError m => m String -> m String
trapError action = catchError action $ return . show


------- Utility Functions to work with Map with LispErrors -------

fromMap :: Ord k => LispError -> k -> M.Map k v -> IOEvaled v
fromMap err key mmap = maybe (throwError err) return $ M.lookup key mmap


------- Helper Functions to Create LispErrors -------

bindingNotFound :: String -> LispError
bindingNotFound = UnboundVar "Cannot find binding"
