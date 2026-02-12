module MonadM where 
import Lang 
import Control.Monad (liftM, ap)
import Data.Map as Map
newtype EvalM a = EvalM 
  { runEval :: Env -> Maybe a }

instance Monad EvalM where
  return t = EvalM $ \_ -> Just t
  m >>= f = EvalM $ \env ->
    case runEval m env of
      Just t -> runEval (f t) env
      Nothing -> Nothing

instance Applicative EvalM where
  pure x = EvalM $ \_ -> Just x
  
  (<*>) f x = EvalM $ \env ->
    case runEval f env of
      Nothing -> Nothing
      Just g ->
        case runEval x env of
          Nothing -> Nothing
          Just v -> Just (g v)  
 

instance Functor EvalM where
  fmap = liftM

getEnv :: EvalM Env 
getEnv = EvalM $ \env -> Just env  

