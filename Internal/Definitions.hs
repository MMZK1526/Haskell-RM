{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Internal.Definitions where

import           Data.Array (Array)
import qualified Data.Array as A
import           Data.Bifunctor (first)
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Array.IO (IOArray)
import           Data.Array.ST (MArray, STArray)
import qualified Data.Array.ST as MA
import           Data.Foldable (toList)
import           Control.Monad (forM_, unless)
import           Control.Monad.ST (ST, runST)
import qualified Gadgets.Array as A
import qualified Gadgets.Array.Mutable as MA
import qualified Gadgets.Array.ST as STA

-- | One line of RM.
data Line = P Int Int | M Int Int Int | H
  deriving Eq

instance Show Line where
  show H          = "ARRÊT"
  show (P r l)    = "R" ++ show r ++ "+ " ++ show l
  show (M r l l') = "R" ++ show r ++ "- " ++ show l ++ " " ++ show l'

-- | The code of a RM consisting of a bunch of "Line"s.
newtype RMCode = RMCode (Array Int Line)
  deriving Eq

instance Show RMCode where
  show (RMCode arr)
    | null arr  = "[EMPTY MACHINE]"
    | otherwise = tail
                $ foldr (\(i, l) l'-> concat ["\nL", show i, ": ", l, l'])
                  "" . zip [0..] $ show <$> toList arr

-- | Gets the number of registers from "RMCode".
argc :: RMCode -> Int
argc (RMCode code) = 1 + foldr max 0 (go <$> code)
  where
    go H         = 0
    go (P i _)   = i
    go (M i _ _) = i

-- | Gets the number of lines from "RMCode"
linec :: RMCode -> Int
linec rmcode@(RMCode code) = max (length code)
                           $ 1 + foldr max (-1) (go <$> code)
  where
    go H         = -1
    go (P _ j)   = j
    go (M _ j k) = max j k

-- | Cycles in a RM code (formed by plus-statements and the main branch of
-- minus-statements).
--
-- It is an array which elements are lists of pairs of "Int" and pairs of
-- "Integer"s. Each pair contains the register modified in the cycle with the
-- pair of its net increment and its largest decrement within the cycle.
--
-- If the current value of the register is no lesser than the third element,
-- it can survive the entire cycle, so we can just go throught the cycle and
-- add with the second element.
type RMCycle = Array Int (Int, [(Int, (Integer, Integer))])

-- Gets the cycle of a "RMCode".
getCycle :: RMCode -> RMCycle
getCycle rmCode@(RMCode code) = runST $ do
  let (_, sup) = A.bounds code
  arrST <- STA.fromList $ replicate (sup + 1) (0, [])
  forM_ [0..sup] $ \i -> arrST MA.=: i $ first length (go i i S.empty M.empty)
  MA.freeze arrST
  where
    merge (tweak, _) (net, dec) = (net + tweak, min (net + tweak) dec)
    go s i set dict
      | s == i && not (S.null set) = (set, M.toList dict)
      | S.member i set             = (S.empty, [])
      | otherwise                  = case code A.! i of
        P n j   -> go s j (S.insert i set)
                          (M.insertWith merge n (1, 1) dict)
        M n j _ -> go s j (S.insert i set)
                          (M.insertWith merge n (-1, -1) dict)
        H       -> (S.empty, [])

-- | The state of a RM under execution.
-- Holding the value of each registers, the cycles, the program counter, the
-- number of steps, if it is stuck in a loop, and if it has reached a halting
-- configuration.
--
-- Note that the state is represented by a mutable array, so this data
-- structure is only useful under the monad "m".
data RMState a m
  =  MArray a Integer m
  => RMState Int Integer Bool Bool RMCycle (a Int Integer)

-- | The result snapshot of a RM.
data RMResult = RMResult { resRegs  :: [Integer]
                         , resPC    :: Int
                         , resSteps :: Integer }
              | RMLoop

type RMStateST s = RMState (STArray s) (ST s)

pattern RMState' :: MArray a Integer m => Int -> Integer -> a Int Integer
                 -> RMState a m
pattern RMState' pc c regs <- RMState pc c _ _ _ regs

-- Contructing a "RMState" under the "ST" monad.
rmStateST :: Int -> Integer -> Bool -> Bool -> RMCycle -> STArray s Int Integer
          -> RMState (STArray s) (ST s)
rmStateST = RMState

-- | A "RMCode" with the current state of registers and program counter.
data RM a m
  =  MArray a Integer m
  => RM { getCode     :: RMCode
        , getRegState :: RMState a m }

-- | Check if the "RM" has terminated or contains loop.
isRMTerminated :: MArray a Integer m => RM a m -> (Bool, Bool)
isRMTerminated rm = let RMState _ _ h l _ _ = getRegState rm in (h, l)
{-# INLINE isRMTerminated #-}

-- | Set the loop flag in the "RM" to "True".
setLoop :: MArray a Integer m => RM a m -> RM a m
setLoop rm = rm { getRegState = RMState pc c h True cycle regs }
  where
    RMState pc c h _ cycle regs = getRegState rm

-- | A bidirectional pattern for "RM" that extracts the array in "RMCode" and
-- the mutable array of register values in "RMState".
pattern RM' :: MArray a Integer m
  => Array Int Line
  -> a Int Integer -> Int -> Integer -> RM a m
pattern RM' code regs pc c <- RM (RMCode code) (RMState pc c _ _ _ regs)
  where
    RM' code regs pc c = let rmCode = RMCode code
                             cycle  = getCycle rmCode
                         in  RM rmCode (RMState pc c False False cycle regs)

-- | Builds "RM" from "RMCode" and a list of arguments (starting with r0) with
-- pc = 0.
initRM :: forall m a. MArray a Integer m => RMCode -> [Integer] -> m (RM a m)
initRM rmCode@(RMCode code) args = do
  let c  = argc rmCode
  let as = take c args ++ replicate (c - length args) 0
  argArr <- MA.fromList as :: m (a Int Integer)
  return $ if length code == linec rmCode
    then RM' code argArr 0 0
    else RM' code' argArr 0 0
    where
      code' = A.fromList $ toList code ++
        replicate (linec rmCode - length code) H

-- | "initRM0" with "STArray".
initRMST :: RMCode -> [Integer] -> ST s (RM (STArray s) (ST s))
initRMST = initRM

-- | "initRM0" with "IOArray".
initRMIO :: RMCode -> [Integer] -> IO (RM IOArray IO)
initRMIO = initRM
