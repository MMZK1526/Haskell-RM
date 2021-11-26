{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RM where

import           Control.Monad (forM, forM_)
import           Control.Monad.ST (ST, runST)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT(..), get, gets, put)
import           Data.Array (Array)
import qualified Data.Array as A
import           Data.Array.ST (MArray, STArray)
import qualified Data.Array.ST as MA
import           Data.Foldable (toList)
import           Definitions
import qualified Gadgets.Array.Mutable as MA

-- | Run the given RMCode with the given list of arguments, returns the list of
-- registers on termination (where r0 is the result).
runRM :: RMCode -> [Integer] -> [Integer]
runRM rmCode args = runST $ do
  rm            <- initRMST rmCode args
  RMState' regs <- exec rm
  init . toList <$> (MA.freeze :: STArray s Int a -> ST s (Array Int a)) regs

-- | Run the given RMCode with the given list of arguments, returns the answer.
evalRM :: RMCode -> [Integer] -> Integer
evalRM = (head .) . runRM

-- | Evaluate the given "RM" by one step.
eval1S :: forall m a. MonadFail m
  => MArray a Integer m
  => StateT (RM a m) m (RMState a m)
eval1S = do
  RM' code regs pc <- get
  RMState c h cy _ <- gets getRegState
  let (_, sup) = A.bounds code
  let halt     = return $ RMState c True cy regs
  if   pc < 0 || pc > sup || h
  then halt
  else case code A.! pc of
    P n i   -> if n >= c
      then halt
      else do
        lift (MA.adjust' regs (+ 1) n) >> put (RM' code regs i)
        return $ RMState c h cy regs
    M n i j -> if n >= c
      then halt
      else do
        x <- lift $ regs MA.! n
        if   x == 0
        then do
          put (RM' code regs j)
          return $ RMState c h cy regs
        else do
          lift $ regs MA.=: n $ x `seq` (x - 1)
          put (RM' code regs i)
          return $ RMState c h cy regs
    H       -> halt

-- | Evaluate the given "RM" by one cycle.
evalCycleS :: forall m a. MonadFail m
  => MArray a Integer m
  => StateT (RM a m) m (RMState a m)
evalCycleS = do
  RM' code regs pc <- get
  RMState c h cy _ <- gets getRegState
  let (_, sup) = A.bounds code
  let cycle    = cy A.! pc
  let negs     = filter ((< 0) . snd . snd) cycle
  if   null negs
  then eval1S
  else do
  rounds <- fmap minimum $ 
    forM negs $ \(r, (_, dec)) -> (`div` (-dec)) <$> lift (regs MA.! r)
  if   rounds == 0
  then eval1S
  else do
  forM_ cycle $ \(r, (net, _)) -> lift (MA.adjust' regs (+ rounds * net) r)
  return $ RMState c h cy regs

-- | Execute the RM until it halts.
exec :: forall m a. MonadFail m
  => MArray a Integer m
  => RM a m -> m (RMState a m)
exec rm = do
  (rs@(RMState _ h _ regs), rm) <- runStateT evalCycleS rm
  if h then return rs else exec rm
