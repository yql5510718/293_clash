module MAC where

import Clash.Prelude
import Clash.Explicit.Testbench

-- Use aregester function to perserve state
-- Answer Begin
mac x = acc
  where
    acc = register 0 (acc + x)
-- Answer End

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Signed 9)
  -> Signal System (Signed 9)
topEntity = exposeClockResetEnable mac

testBench :: Signal System Bool
testBench = done
  where
    testInput    = stimuliGenerator clk rst $(listToVecTH [1 :: Signed 9,2,3,4])
    expectOutput = outputVerifier' clk rst $(listToVecTH [0 :: Signed 9,1,3,6,10,10,10])
    done         = expectOutput (topEntity clk rst en testInput)
    en           = enableGen
    clk          = tbSystemClockGen (not <$> done)
    rst          = systemResetGen
