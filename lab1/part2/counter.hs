module MAC where

import Clash.Prelude
import Clash.Explicit.Testbench

-- Use regEn function to make counter

-- counter_en :: Signal System (Bool) -> Signal System (Signed 9)
-- Answer Begin
counter_en x = count
  where
    count     = regEn 0 x (count + 1)
-- Answer End

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Bool)
  -> Signal System (Signed 9)
topEntity = exposeClockResetEnable counter_en

testBench :: Signal System Bool
testBench = done
  where
    testInput    = stimuliGenerator clk rst $(listToVecTH [True :: Bool ,False, False, False,True])
    expectOutput = outputVerifier' clk rst $(listToVecTH [0 :: Signed 9,1,1,1,1,2,2])
    done         = expectOutput (topEntity clk rst en testInput)
    en           = enableGen
    clk          = tbSystemClockGen (not <$> done)
    rst          = systemResetGen
