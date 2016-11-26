{-# LANGUAGE TupleSections
           , FlexibleContexts
  #-}

module Variable
  ( runIOEvaled
  , runIOEvaledSafe
  , emptyEnv
  , liftEvaled
  , runEvaled
  , bindingNotFound

  , getVar
  , getVarDeep

  , setVar
  , defineVar
  , bindVars
  , mBindVars

  , derefPtrValSafe
  , derefPtrVal
  , modifyPtrVal
  , toPtrVal

  , isBound
  , getBindings
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
  where
    trapError :: IOEvaled String -> IOEvaled String
    trapError action = catchError action showLispErrorSafe

    showLispErrorSafe :: LispError -> IOEvaled String
    showLispErrorSafe (NumArgs str vals)       = show . NumArgs str <$> traverse derefPtrValSafe vals
    showLispErrorSafe (TypeMismatch str val)   = show . TypeMismatch str <$> derefPtrValSafe val
    showLispErrorSafe (BadSpecialForm str val) = show . BadSpecialForm str <$> derefPtrValSafe val
    showLispErrorSafe (ImmutableArg str val)   = show . ImmutableArg str <$> derefPtrValSafe val
    showLispErrorSafe (InvalidArgs str vals)   = show . InvalidArgs str <$> traverse derefPtrValSafe vals
    showLispErrorSafe e                        = return $ show e

-- Check if a variable has a binding in the environment
isBound :: VarName -> Env -> IO Bool
isBound var env = M.member var <$> readIORef env

derefPtrValSafe :: LispVal -> IOEvaled LispVal
derefPtrValSafe val@(LPointer _)    = derefPtrVal val >>= derefPtrValSafe
derefPtrValSafe (LList vals)        = LList       <$> mapM derefPtrValSafe vals
derefPtrValSafe (LDottedList hd tl) = LDottedList <$> mapM derefPtrValSafe hd   <*> derefPtrValSafe tl
derefPtrValSafe (LVector vec)       = LVector     <$> mapM derefPtrValSafe vec
derefPtrValSafe val                 = return val

derefPtrVal :: LispVal -> IOEvaled LispVal
derefPtrVal (LPointer ptrVal) = liftIO $ readIORef ptrVal
derefPtrVal val               = throwError $ ImmutableArg "Not pointer value" val

{-
 - Gets the variable from the environment
 - - read environment into 'IO' monad, then lift into the current monad
 -   transformer stack
 - - lookup the variable in the environment:
 -   - if found, read from 'IORef' to 'IO' then lift back into the current
 -     stack
 -   - 'UnboundVar' error otherwise
 -}
getVar :: VarName -> Env -> IOEvaled LispVal
getVar var env = do
    vars <- liftIO $ readIORef env
    fromMap notFoundErr var vars
  where notFoundErr :: LispError
        notFoundErr = bindingNotFound var

getVarDeep :: VarName -> Env -> IOEvaled LispVal
getVarDeep var env = do
    vars    <- liftIO $ readIORef env
    lispVal <- fromMap notFoundErr var vars
    case lispVal of
      LPointer ptr -> liftIO $ readIORef ptr
      _            -> return lispVal

  where notFoundErr :: LispError
        notFoundErr = bindingNotFound var

getBindings :: Env -> IO VarBinding
getBindings = readIORef

setVar :: VarName -> LispVal -> Env -> IOEvaled LispVal
setVar var value env = do
    defined <- liftIO $ isBound var env
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

defineVar :: VarName -> LispVal -> Env -> IO LispVal
defineVar var value env = env `modifyIORef` M.insert var value >> return value

-- Updates/Mutates current environment with all of the new bindings
mBindVars :: Env -> [(VarName, LispVal)] -> IO Env
mBindVars env bindings = modifyIORef env (extendEnv bindings) >> return env

extendEnv :: [(VarName, LispVal)] -> VarBinding -> VarBinding
extendEnv bindings envMap = foldr addToMap envMap bindings
  where addToMap :: Ord k => (k, v) -> M.Map k v -> M.Map k v
        addToMap = uncurry M.insert

-- Creates a new environment with old and all of the new bindings
bindVars :: Env -> [(VarName, LispVal)] -> IO Env
bindVars env bindings = extendEnv bindings <$> readIORef env >>= newIORef

toPtrVal :: LispVal -> IO LispVal
toPtrVal = fmap LPointer . newIORef


------- Utility Functions to work with Map with LispErrors -------

fromMap :: Ord k => LispError -> k -> M.Map k v -> IOEvaled v
fromMap err key mmap = maybe (throwError err) return $ M.lookup key mmap


------- Helper Functions to Create LispErrors -------

bindingNotFound :: String -> LispError
bindingNotFound = UnboundVar "Cannot find binding"
