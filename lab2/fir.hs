module FIR where

import Clash.Prelude
import Clash.Explicit.Testbench

-- should use 'fold' and 'zipWith' function to calculte dot product
-- purely Haskell
dotp :: SaturatingNum a
     => Vec (n + 1) a
     -> Vec (n + 1) a
     -> a
-- Answer Begin
dotp as bs = fold boundedAdd (zipWith boundedMul as bs)
-- Answer End

-- should use 'window' function to determine current window size
fir
  :: ( HiddenClockResetEnable tag
     , Default a
     , KnownNat n
     , SaturatingNum a
     , NFDataX a )
  => Vec (n + 1) a -> Signal tag a -> Signal tag a
  -- Answer Begin
fir coeffs x_t = y_t
  where
    y_t = dotp coeffs <$> bundle xs
    xs  = window x_t
-- Answer End

topEntity
  :: Clock  System
  -> Reset  System
  -> Enable System
  -> Signal System (Signed 16)
  -> Signal System (Signed 16)
topEntity = exposeClockResetEnable (fir (2:>3:>(-2):>8:>Nil)) -- inferred int
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst (2:>3:>(-2):>7:>Nil)
    expectedOutput = outputVerifier' clk rst (4:>12:>1:>18:>Nil)
    done           = expectedOutput (topEntity clk rst (enableGen) testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
