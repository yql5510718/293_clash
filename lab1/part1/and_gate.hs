module AND where

import Clash.Prelude
import Clash.Explicit.Testbench

-- Truth table
-- I | I | O
-- 0 | 0 | 0
-- 0 | 1 | 0
-- 1 | 0 | 0
-- 1 | 1 | 1
-- Use unbundle to get two seperate io input

andGate :: Signal System (Bool, Bool) -> Signal System Bool
andGate input = output
-- Answer Begin
  where
    (x, y) = unbundle input
    output = x .&&. y
--Answer End

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Bool, Bool)
  -> Signal System (Bool)
topEntity = exposeClockResetEnable andGate

testBench :: Signal System Bool
testBench = done
  where
    testInput    = stimuliGenerator clk rst $(listToVecTH [(False, False) :: (Bool,Bool),(False, True),(True,False),(True,True)])
    expectOutput = outputVerifier' clk rst $(listToVecTH [False :: Bool, False,False,True])
    done         = expectOutput (topEntity clk rst en testInput)
    en           = enableGen
    clk          = tbSystemClockGen (not <$> done)
    rst          = systemResetGen
