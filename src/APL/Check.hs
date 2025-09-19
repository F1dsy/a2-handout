module APL.Check (checkExp, Error) where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

type Error = String

type Scope = [VName]

newtype CheckM a = CheckM (Scope -> Either Error a)

--Implementing the nessecities for the monad function.
instance Functor CheckM where
  fmap = liftM

instance Applicative CheckM where
  pure x = CheckM $ \_scope -> Right x
  (<*>) = ap

instance Monad CheckM where
  CheckM x >>= f = CheckM $ \scope ->
    case x scope of
      Right a ->
        let (CheckM y) = f a
         in y scope
      Left err -> Left err
-- looks at current scope
askScope :: CheckM Scope
askScope = CheckM $ \scope -> Right scope
--used for local scope subcomputations
localScope :: (Scope -> Scope) -> CheckM a -> CheckM a
localScope f (CheckM m) = CheckM $ \scope -> m (f scope)
--checks validity of expression in current scope
check :: Exp -> CheckM ()
check (Var a) = do
  scope <-
    askScope
  if a `elem` scope
    then pure ()
    else CheckM $ \_ -> Left ("Variable not in scope: " ++ a)
check (Let var e1 e2) =
  check e1 >> localScope (var :) (check e2)
check (Lambda var body) =
  localScope (var :) $ check body
check _ = pure ()

--runs the checker
checkExp :: Exp -> Maybe Error
checkExp e =
  either Just (const Nothing) (v [])
  where
    (CheckM v) = check e
