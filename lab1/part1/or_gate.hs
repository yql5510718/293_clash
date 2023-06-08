module OR where

import Clash.Prelude
import Clash.Explicit.Testbench

-- Truth table
-- I | I | O
-- 0 | 0 | 0
-- 0 | 1 | 1
-- 1 | 0 | 1
-- 1 | 1 | 1
-- Use unbundle to get two seperate io input

orGate :: Signal System (Bool, Bool) -> Signal System Bool
orGate input = output
-- Answer Begin
  where
    (x, y) = unbundle input
    output = x .||. y
-- Answer end

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Bool, Bool)
  -> Signal System (Bool)
topEntity = exposeClockResetEnable orGate

testBench :: Signal System Bool
testBench = done
  where
    testInput    = stimuliGenerator clk rst $(listToVecTH [(False, False) :: (Bool,Bool), (False, True), (True,False), (True,True)])
    expectOutput = outputVerifier' clk rst $(listToVecTH [False :: Bool, True, True, True])
    done         = expectOutput (topEntity clk rst en testInput)
    en           = enableGen
    clk          = tbSystemClockGen (not <$> done)
    rst          = systemResetGen
