{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

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
import           Data.Array.ST (MArray, STArray, getBounds)
import qualified Data.Array.ST as MA
import           Data.Bifunctor (bimap)
import           Data.Foldable (toList)
import qualified Gadgets.Array.Mutable as MA
import           Gadgets.IO
import           Internal.Definitions

-- | Run the given RMCode with the given list of arguments (starting with r0),
-- returns the list of registers on termination (where r0 is the result).
runRM :: RMCode -> [Integer] -> RMResult
runRM rmCode args = runST $ do
  mrm <- initRMST rmCode args >>= exec
  case mrm of
    Nothing -> pure RMLoop
    Just rm -> do
      let RMState' pc c regs = getRegState rm
      regs' <- toList <$> freeze regs
      return $ RMResult regs' pc c
  where
    freeze  = MA.freeze :: STArray s Int a -> ST s (Array Int a)
    exec rm = do
      rm <- execStateT evalCycleS rm
      case isRMTerminated rm of
        (_, True) -> pure Nothing
        (True, _) -> pure $ Just rm
        _         -> exec rm

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
      | i == s + n = pure (rm, fst $ isRMTerminated rm)
      | otherwise  = do
        let rs@(RMState' pc c regs) = getRegState rm
        regs                               <- freeze regs
        putStrLn $ "Step " ++ show i ++ ": "
        putStrLn $ "PC: " ++ show pc
        forM_ (A.assocs regs) $ \(i, r) -> putStrLn $ "R" ++ show i
                                        ++ ": " ++ show r
        putLn
        rm <- execStateT eval1S rm
        if fst $ isRMTerminated rm then return (rm, True) else execIO rm $ i + 1

-- | Evaluate the given "RM" by one step.
eval1S :: forall m a. MonadFail m => MArray a Integer m => StateT (RM a m) m ()
eval1S = do
  RM' code regs pc _    <- get
  RMState _ c' h l cy _ <- gets getRegState
  (_, maxReg)           <- lift $ getBounds regs
  let (_, sup) = A.bounds code
  let halt     = modify (\rm -> rm { getRegState = RMState pc (c' + 1)
                                                           True l cy regs })
  if   pc < 0 || pc > sup || h
  then halt
  else case code A.! pc of
    P n i   -> if n > maxReg
      then halt
      else lift (MA.adjust' regs (+ 1) n) >> put (RM' code regs i (c' + 1))
    M n i j -> if n > maxReg
      then halt
      else do
        x <- lift $ regs MA.! n
        if   x == 0
        then put (RM' code regs j (c' + 1))
        else do
          lift $ regs MA.=: n $ x `seq` (x - 1)
          put (RM' code regs i (c' + 1))
    H       -> halt

-- | Evaluate the given "RM" by one cycle. If infinite loop is detected,
-- terminate the execution immediately.
evalCycleS :: forall m a. MonadFail m => MArray a Integer m
           => StateT (RM a m) m ()
evalCycleS = do
  RM' code regs pc _    <- get
  RMState pc c h l cy _ <- gets getRegState
  let (_, sup)    = A.bounds code
  let (cL, cycle) = cy A.! pc
  let negs        = filter ((< 0) . snd . snd) cycle
  if   null negs
  then if null cycle then eval1S else modify setLoop
  else do
    (b, rounds) <- fmap (bimap minimum minimum . unzip) . forM negs
                 $ \(r, (net, dec)) -> (net ,) . (`div` (-dec)) 
                                   <$> lift (regs MA.! r)
    case (b >= 0, rounds) of
      (_, 0)    -> eval1S
      (True, _) -> modify setLoop
      _         -> do
        forM_ cycle
          $ \(r, (net, _)) -> lift (MA.adjust' regs (+ rounds * net) r)
        let c' = c + fromIntegral cL * rounds
        modify (\s -> s { getRegState = RMState pc c' h l cy regs })
