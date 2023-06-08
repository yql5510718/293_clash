module XOR where

import Clash.Prelude
import Clash.Explicit.Testbench

-- Truth table
-- I | I | O
-- 0 | 0 | 0
-- 0 | 1 | 1
-- 1 | 0 | 1
-- 1 | 1 | 0
-- Use unbundle to get two seperate io input


xorGate :: Signal System (Bool, Bool) -> Signal System Bool
xorGate input = output
-- Answer Begin
  where
    (x, y) = unbundle input
    output = (x .&&. not <$> y) .||. (not <$> x .&&. y)
-- Answer End

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Bool, Bool)
  -> Signal System (Bool)
topEntity = exposeClockResetEnable xorGate

testBench :: Signal System Bool
testBench = done
  where
    testInput    = stimuliGenerator clk rst $(listToVecTH [(False, False) :: (Bool,Bool), (False, True), (True,False), (True,True)])
    expectOutput = outputVerifier' clk rst $(listToVecTH [False :: Bool, True, True, False])
    done         = expectOutput (topEntity clk rst en testInput)
    en           = enableGen
    clk          = tbSystemClockGen (not <$> done)
    rst          = systemResetGen
