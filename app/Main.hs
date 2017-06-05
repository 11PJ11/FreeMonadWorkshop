{-# LANGUAGE DeriveFunctor #-}

module Main where

import           Control.Monad.Free

data BoolOperation next =
    AND Bool Bool (Bool -> next)
  | OR Bool Bool (Bool -> next)
  | NOT Bool (Bool -> next)
  | RETURN Bool
-- -- Not needed with deriving (Functor)
-- instance Functor BoolOperation where
--    fmap f (AND b1 b2 g) = AND b1 b2 (f . g)
--    fmap f (OR b1 b2 g)  = OR b1 b2(f . g)
--    fmap f (NOT b1 g)    = NOT b1 (f . g)
  deriving (Functor)

type Program = Free BoolOperation

and' :: Bool -> Bool -> Program Bool
and' b1 b2 = liftF $ AND b1 b2 id

or' :: Bool -> Bool -> Program Bool
or' b1 b2 = liftF $ OR b1 b2 id

not' :: Bool -> Program Bool
not' b1 = liftF $ NOT b1 id

return' :: Bool -> Program Bool
return' b1 = liftF $ RETURN b1

-- Pure Interpreter
run :: Program Bool -> Bool
run prog = case prog of
  Free (AND b1 b2 next) -> run (next(b1 && b2))
  Free (OR b1 b2 next)  -> run (next(b1 || b2))
  Free (NOT b next)     -> run (next(not b))
  Free (RETURN b)       -> b
  Pure b                -> b

-- Pure Interpreter with logging
runLog :: Program Bool -> [String] -> [String]
runLog prog logs = do
  let res = run prog
  case (prog, logs) of
    (Free (AND b1 b2 next), ls) -> do
      (show b1 ++ " and " ++ show b2 ++ " = " ++ show res):(runLog (next(res)) ls)
    (Free (OR b1 b2 next), ls) -> do
      (show b1 ++ " or " ++ show b2 ++ " = " ++ show res):(runLog (next(res)) ls)
    (Free (NOT b1 next), ls) -> do
      ("not " ++ show b1++ " = " ++ show res):(runLog (next(res)) ls)
    (Free (RETURN b), ls) ->
      ("return " ++ show b):ls
    (Pure b, ls) -> []

-- Insert parser in HERE
-- The parser is something that
-- reads a String and creates a
-- Program of Bool
instructions :: Program Bool
instructions = do
  false <- not' True
  trueOrfalse <- or' True false
  res <- and' false trueOrfalse
  return' res

main :: IO ()
main = do
  let execution = runLog instructions []
  mapM_ (putStrLn . show) execution
  -- putStrLn . show $ run instructions
