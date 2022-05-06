{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Internal.RM where

import           Control.Monad (forM, forM_)
import           Control.Monad.Fail (MonadFail)
import           Control.Monad.ST (ST, runST)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT(..), modify, get, gets, put)
import           Data.Array (Array)
import           Data.Array.IO (IOArray)
import qualified Data.Array as A
import           Data.Array.ST (MArray, STArray)
import qualified Data.Array.ST as MA
import           Data.Foldable (toList)
import qualified Gadgets.Array.Mutable as MA
import           Gadgets.IO
import           Internal.Definitions

-- | Run the given RMCode with the given list of arguments, returns the list of
-- registers on termination (where r0 is the result).
runRM0 :: RMCode -> [Integer] -> RMResult
runRM0 rmCode args = runST $ do
  rm                 <- initRMST0 rmCode args
  RMState' pc c regs <- exec rm
  regs'              <- toList <$> (MA.freeze :: STArray s Int a -> ST s (Array Int a)) regs
  return $ RMResult regs' pc c

-- | Runs the given RMCode with the given list of arguments, returns the answer
-- and prints each steps.
runRMIO0 :: RMCode -> [Integer] -> IO RMResult
runRMIO0 rmCode args =  do
  rm                 <- initRMIO0 rmCode args
  RMState' pc c regs <- execIO rm 1
  regs'              <- toList <$> (MA.freeze :: IOArray Int a -> IO (Array Int a)) regs
  return $ RMResult regs' pc c
  where
    execIO rm i = do
      (rs@(RMState _ pc c h _ regs), rm) <- runStateT eval1S rm
      regs <- (MA.freeze :: IOArray Int a -> IO (Array Int a)) regs
      putStrLn $ "Step " ++ show i ++ ": " 
      putStrLn $ "PC: " ++ show pc
      putStrLn $ "Regs: " ++ show (A.assocs regs)
      putLn
      if h then return rs else execIO rm $ i + 1

-- | Evaluate the given "RM" by one step.
eval1S :: forall m a. MonadFail m
  => MArray a Integer m
  => StateT (RM a m) m (RMState a m)
eval1S = do
  RM' code regs pc _    <- get
  RMState c _ c' h cy _ <- gets getRegState
  let (_, sup) = A.bounds code
  let halt     = return $ RMState c pc (c' + 1) True cy regs
  if   pc < 0 || pc > sup || h
  then halt
  else case code A.! pc of
    P n i   -> if n >= c
      then halt
      else do
        lift (MA.adjust' regs (+ 1) n) >> put (RM' code regs i (c' + 1))
        return $ RMState c i (c' + 1) h cy regs
    M n i j -> if n >= c
      then halt
      else do
        x <- lift $ regs MA.! n
        if   x == 0
        then do
          put (RM' code regs j (c' + 1))
          return $ RMState c j (c' + 1) h cy regs
        else do
          lift $ regs MA.=: n $ x `seq` (x - 1)
          put (RM' code regs i (c' + 1))
          return $ RMState c i (c' + 1) h cy regs
    H       -> halt

-- | Evaluate the given "RM" by one cycle.
evalCycleS :: forall m a. MonadFail m
  => MArray a Integer m
  => StateT (RM a m) m (RMState a m)
evalCycleS = do
  RM' code regs pc _     <- get
  RMState c pc c' h cy _ <- gets getRegState
  let (_, sup)    = A.bounds code
  let (cL, cycle) = cy A.! pc
  let negs        = filter ((< 0) . snd . snd) cycle
  if   null negs
  then eval1S
  else do
    rounds <- fmap minimum $ 
      forM negs $ \(r, (_, dec)) -> (`div` (-dec)) <$> lift (regs MA.! r)
    if   rounds == 0
    then eval1S
    else do
      forM_ cycle $ \(r, (net, _)) -> lift (MA.adjust' regs (+ rounds * net) r)
      let c'' = c' + fromIntegral cL * rounds
      modify (\s -> s { getRegState = RMState c pc c'' h cy regs })
      return $ RMState c pc c'' h cy regs

-- | Execute the RM until it halts with cycle-detection optimisation.
exec :: forall m a. MonadFail m
  => MArray a Integer m
  => RM a m -> m (RMState a m)
exec rm = do
  (rs@(RMState _ _ _ h _ regs), rm) <- runStateT evalCycleS rm
  if h then return rs else exec rm

-- | Naively Execute the RM until it halts without optimisation.
exec' :: forall m a. MonadFail m
  => MArray a Integer m
  => RM a m -> m (RMState a m)
exec' rm = do
  (rs@(RMState _ _ _ h _ regs), rm) <- runStateT eval1S rm
  if h then return rs else exec' rm
