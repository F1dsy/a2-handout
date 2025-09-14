module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup = lookup

type Error = String

type State = ([String], [(Val, Val)])

newtype EvalM a = EvalM (Env -> State -> (State, Either Error a))

instance Functor EvalM where
  fmap = liftM

instance Applicative EvalM where
  pure x = EvalM $ \_env s -> (s, Right x)
  (<*>) = ap

instance Monad EvalM where
  EvalM x >>= f = EvalM $ \env s ->
    case x env s of
      (s', Left err) -> (s', Left err)
      (s', Right x') ->
        let EvalM y = f x'
         in y env s'

askEnv :: EvalM Env
askEnv = EvalM $ \env s -> (s, Right env)

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env -> m (f env)

failure :: String -> EvalM a
failure s = EvalM $ \_env st -> (st, Left s)

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env s ->
  case m1 env s of
    (s', Left _) -> m2 env s'
    (s', Right x) -> (s', Right x)

--  traceShow ("Before: " ++ show env) $
runEval :: EvalM a -> ([String], Either Error a)
-- a : (generic type)
-- m :: Env -> State -> (State, Either Error a)
-- runEval (EvalM m) = m envEmpty ()
runEval (EvalM m) =
  let ((st, _), v) = m envEmpty ([], [])
   in (st, v)

evalPrint :: String -> EvalM ()
evalPrint str = EvalM $ \_env s -> ((fst s ++ [str], snd s), Right ())

evalKvGet :: Val -> EvalM Val
evalKvGet k = EvalM $ \_env (st, kv) ->
  let v = lookup k kv
   in ( (st, kv),
        case v of
          Just v' -> Right v'
          Nothing -> Left $ "Invalid key: " ++ show k
      )

evalKvPut :: Val -> Val -> EvalM ()
evalKvPut k v = EvalM $ \_env (st, kv) -> ((st, (k, v) : kv), Right ())

evalIntBinOp :: (Integer -> Integer -> EvalM Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp f e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> ValInt <$> f x y
    (_, _) -> failure "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp' f = evalIntBinOp f'
  where
    f' x y = pure $ f x y

eval :: Exp -> EvalM Val
eval (CstInt x) = pure $ ValInt x
eval (CstBool b) = pure $ ValBool b
eval (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v
eval (Add e1 e2) = evalIntBinOp' (+) e1 e2
eval (Sub e1 e2) = evalIntBinOp' (-) e1 e2
eval (Mul e1 e2) = evalIntBinOp' (*) e1 e2
eval (Div e1 e2) = evalIntBinOp checkedDiv e1 e2
  where
    checkedDiv _ 0 = failure "Division by zero"
    checkedDiv x y = pure $ x `div` y
eval (Pow e1 e2) = evalIntBinOp checkedPow e1 e2
  where
    checkedPow x y =
      if y < 0
        then failure "Negative exponent"
        else pure $ x ^ y
eval (Eql e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> pure $ ValBool $ x == y
    (ValBool x, ValBool y) -> pure $ ValBool $ x == y
    (_, _) -> failure "Invalid operands to equality"
eval (If cond e1 e2) = do
  cond' <- eval cond
  case cond' of
    ValBool True -> eval e1
    ValBool False -> eval e2
    _ -> failure "Non-boolean conditional."
eval (Let var e1 e2) = do
  v1 <- eval e1
  localEnv (envExtend var v1) $ eval e2
eval (ForLoop (loopparam, initial) (iv, bound) body) = do
  initial_v <- eval initial
  bound_v <- eval bound
  case bound_v of
    ValInt bound_int ->
      loop 0 bound_int initial_v
    _ ->
      failure "Non-integral loop bound"
  where
    loop i bound_int loop_v
      | i >= bound_int = pure loop_v
      | otherwise = do
          loop_v' <-
            localEnv (envExtend iv (ValInt i) . envExtend loopparam loop_v) $
              eval body
          loop (succ i) bound_int loop_v'
eval (Lambda var body) = do
  env <- askEnv
  pure $ ValFun env var body
eval (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValFun f_env var body, arg) ->
      localEnv (const $ envExtend var arg f_env) $ eval body
    (_, _) ->
      failure "Cannot apply non-function"
eval (TryCatch e1 e2) =
  eval e1 `catch` eval e2
eval (Print str e1) = do
  e <- eval e1
  let str' =
        str
          ++ ": "
          ++ ( case e of
                 ValInt n -> show n
                 ValBool b -> show b
                 ValFun {} -> "#<fun>"
             )
  evalPrint str'
  pure e
eval (KvGet a) = do
  a' <- eval a
  evalKvGet a'
eval (KvPut k v) = do
  k' <- eval k
  v' <- eval v
  evalKvPut k' v'
  pure v'
