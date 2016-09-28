{-# LANGUAGE TupleSections #-}

module Variable where

import Control.Monad.Except
import Control.Monad.ST
import Data.STRef
import Data.Maybe (isJust)

import Definition
import LispError

type VarName      = String
type VarBinding s = (VarName, STRef s LispVal)
type Env s        = STRef s [VarBinding s]
type STEvaled s   = ExceptT LispError (ST s)

{-
 - Signatures of the commonly used 'STRef' functions below:
 -   newSTRef   :: a              -> ST (STRef s a)
 -   readSTRef  :: STRef s a      -> ST a
 -   writeSTRef :: STRef s a -> a -> ST ()
 -}
emptyEnv :: ST s (Env s)
emptyEnv = newSTRef []

liftEvaled :: Evaled a -> STEvaled s a
liftEvaled = either throwError return . runExcept

runSTEvaled :: (LispError -> b) -> (a -> b) -> STEvaled s a -> ST s b
runSTEvaled errHandler sucHandler = fmap (either errHandler sucHandler)
                                  . runExceptT

runSTEvaledStr :: STEvaled s String -> ST s String
runSTEvaledStr = fmap (either undefined id)
               . runExceptT
               . trapError

-- Check if a variable has a binding in the environment
isBound :: Env s -> VarName -> ST s Bool
isBound envRef var = isJust . lookup var <$> readSTRef envRef

{-
 - Gets the variable from the environment
 - - read environment into 'ST' monad, then lift into the current monad
 -   transformer stack
 - - lookup the variable in the environment:
 -   - if found, read from 'STRef' to 'ST' then lift back into the current
 -     stack
 -   - 'UnboundVar' error otherwise
 -}
getVar :: Env s -> VarName -> STEvaled s LispVal
getVar envRef var = do
  env <- lift $ readSTRef envRef
  maybe (throwError $ UnboundVar "Could not find binding for" var)
        (lift . readSTRef)
        (lookup var env)

setVar :: Env s -> VarName -> LispVal -> STEvaled s LispVal
setVar envRef var value = do
  env <- lift $ readSTRef envRef
  maybe (throwError $ UnboundVar "Setting an unbound variable" var)
        (lift . (`writeSTRef` value))
        (lookup var env)

  -- Returns the set value for convenience
  return value

{-
 - Note: If var binding is not previously defined:
 -       - creates a 'ST' action
 -       - create new 'STRef' for the value to be defined with
 -       - read the current environment
 -       - write the "(var binding, value STRef)" pair into environment
 -       - lift plain 'ST' action back into the monad transformer stack
 -}
defineVar :: Env s -> VarName -> LispVal -> STEvaled s LispVal
defineVar envRef var value = do
  alreadyDefined <- lift $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value
    else lift $ do
      valueRef <- newSTRef value
      env      <- readSTRef envRef
      writeSTRef envRef $ (var, valueRef):env
      return value

bindVars :: Env s -> [(VarName, LispVal)] -> ST s (Env s)
bindVars envRef bindings = readSTRef envRef >>= extendEnv bindings >>= newSTRef
  where extendEnv :: [(VarName, LispVal)]
                  -> [VarBinding s]
                  -> ST s [VarBinding s]
        extendEnv bindings env = (++ env) <$> mapM addBinding bindings

        addBinding :: (VarName, LispVal) -> ST s (VarBinding s)
        addBinding (var, value) = (var,) <$> newSTRef value
