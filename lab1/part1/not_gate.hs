module NOT where

import Clash.Prelude
import Clash.Explicit.Testbench

notGate :: Signal System Bool -> Signal System Bool
notGate input = output
-- Answer Begin
  where
    output = not <$> input
-- Answer end

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System Bool
  -> Signal System Bool
topEntity = exposeClockResetEnable notGate

testBench :: Signal System Bool
testBench = done
  where
    testInput    = stimuliGenerator clk rst $(listToVecTH [False :: Bool, False,True])
    expectOutput = outputVerifier' clk rst $(listToVecTH [True :: Bool, True, False])
    done         = expectOutput (topEntity clk rst en testInput)
    en           = enableGen
    clk          = tbSystemClockGen (not <$> done)
    rst          = systemResetGen
