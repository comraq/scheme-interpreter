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
  , setVar
  , defineVar
  , bindVars
  ) where

import Control.Monad.Except
import Data.IORef
import Data.Maybe (isJust)

import Definition


{-
 - Signatures of the commonly used 'IORef' functions below:
 -   newIORef   :: a            -> IO (IORef a)
 -   readIORef  :: IORef a      -> IO a
 -   writeIORef :: IORef a -> a -> IO ()
 -}
emptyEnv :: IO Env
emptyEnv = newIORef []

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
isBound envRef var = isJust . lookup var <$> readIORef envRef

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
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ bindingNotFound var)
        (liftIO . readIORef)
        (lookup var env)

setVar :: Env -> VarName -> LispVal -> IOEvaled LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting an unbound variable" var)
        (liftIO . (`writeIORef` value))
        (lookup var env)

  -- Returns the set value for convenience
  return value

{-
 - Note: If var binding is not previously defined:
 -       - creates a 'IO' action
 -       - create new 'IORef' for the value to be defined with
 -       - read the current environment
 -       - write the "(var binding, value IORef)" pair into environment
 -       - lift plain 'IO' action back into the monad transformer stack
 -}
defineVar :: Env -> VarName -> LispVal -> IOEvaled LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value
    else liftIO $ do
      valueRef <- newIORef value
      env      <- readIORef envRef
      writeIORef envRef $ (var, valueRef):env
      return value

bindVars :: Env -> [(VarName, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv :: [(VarName, LispVal)]
                  -> [VarBinding]
                  -> IO [VarBinding]
        extendEnv bindings env = (++ env) <$> mapM addBinding bindings

        addBinding :: (VarName, LispVal) -> IO VarBinding
        addBinding (var, value) = (var,) <$> newIORef value

trapError :: MonadError LispError m => m String -> m String
trapError action = catchError action $ return . show



------- Helper Functions to Create LispErrors -------

bindingNotFound :: String -> LispError
bindingNotFound = UnboundVar "Cannot find binding"
