{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Internal.RM where

import           Control.Monad (forM, forM_)
import           Control.Monad.Fail (MonadFail)
import           Control.Monad.ST (ST, runST)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State
  (StateT(..), execStateT, modify, get, gets, put)
import           Data.Array (Array)
import           Data.Array.IO (IOArray)
import qualified Data.Array as A
import           Data.Array.ST (MArray, STArray)
import qualified Data.Array.ST as MA
import           Data.Foldable (toList)
import qualified Gadgets.Array.Mutable as MA
import           Gadgets.IO
import           Internal.Definitions

-- | Run the given RMCode with the given list of arguments (starting with r0),
-- returns the list of registers on termination (where r0 is the result).
runRM :: RMCode -> [Integer] -> RMResult
runRM rmCode args = runST $ do
  rm    <- initRMST rmCode args >>= exec
  let RMState' pc c regs = getRegState rm
  regs' <- toList <$> freeze regs
  return $ RMResult regs' pc c
  where
    freeze  = MA.freeze :: STArray s Int a -> ST s (Array Int a)
    exec rm = do
      rm <- execStateT evalCycleS rm
      if isRMTerminated rm then return rm else exec rm

-- | Runs the given  "RM". Returns the answer and prints each steps. Run at most
-- "n" iterations.
runRMIO :: RM IOArray IO -> (Int, Int) -> IO (RM IOArray IO, Maybe RMResult)
runRMIO rm (s, n) = do
  (rm, h) <- execIO rm s
  if   h
  then do
    let RMState' pc c regs = getRegState rm
    regs' <- toList <$> freeze regs
    return (rm, Just $ RMResult regs' pc c)
  else return (rm, Nothing)
  where
    freeze = MA.freeze :: IOArray Int a -> IO (Array Int a)
    execIO rm i
      | i == s + n = pure (rm, isRMTerminated rm)
      | otherwise  = do
        let rs@(RMState' pc c regs) = getRegState rm
        regs                               <- freeze regs
        putStrLn $ "Step " ++ show i ++ ": "
        putStrLn $ "PC: " ++ show pc
        putStrLn $ "Regs: " ++ show (A.assocs regs)
        putLn
        rm <- execStateT eval1S rm
        if isRMTerminated rm then return (rm, True) else execIO rm $ i + 1

-- | Evaluate the given "RM" by one step.
eval1S :: forall m a. MonadFail m => MArray a Integer m => StateT (RM a m) m ()
eval1S = do
  RM' code regs pc _    <- get
  RMState c _ c' h cy _ <- gets getRegState
  let (_, sup) = A.bounds code
  let halt     = modify (\rm -> rm { getRegState = RMState c pc (c' + 1)
                                                           True cy regs })
  if   pc < 0 || pc > sup || h
  then halt
  else case code A.! pc of
    P n i   -> if n >= c
      then halt
      else lift (MA.adjust' regs (+ 1) n) >> put (RM' code regs i (c' + 1))
    M n i j -> if n >= c
      then halt
      else do
        x <- lift $ regs MA.! n
        if   x == 0
        then put (RM' code regs j (c' + 1))
        else do
          lift $ regs MA.=: n $ x `seq` (x - 1)
          put (RM' code regs i (c' + 1))
    H       -> halt

-- | Evaluate the given "RM" by one cycle.
evalCycleS :: forall m a. MonadFail m => MArray a Integer m => StateT (RM a m) m ()
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
