{-# LANGUAGE RecordWildCards #-} 
module Output.Test2 where 
import Sprockell.System
prog:: [Instruction] 
prog = [Const 3 RegA
        ,Compute Add RegA PC RegA
        ,Store RegA (Addr 0)            -- task TestH starts at 9
        ,Jump (Rel 13)                  -- skip TestH: 21
        ,Pop RegA
        ,Store RegA (Addr 1)            -- n = 12
        ,Load (Addr 1) RegA
        ,Store RegA (Addr 2)            -- a = n = 12
        ,Load (Addr 2) RegA
        ,Const 1 RegB
        ,Compute Add RegA RegB RegA
        ,Store RegA (Addr 2)            -- a = a + 1 = 13
        ,Load (Addr 2) RegA
        ,Pop RegB                       -- pop return address
        ,Push RegA                      -- push return value
        ,Jump (Ind RegB)                -- jump to return address: 46
        ,Const 3 RegA
        ,Compute Add RegA PC RegA
        ,Store RegA (Addr 1)            -- task Test starts at 25
        ,Jump (Rel 36)                  -- skip Test: 60
        ,Pop RegA
        ,Store RegA (Addr 2)            -- h = 3
        ,Const 4 RegA
        ,Load (Addr 2) RegB
        ,Compute Mul RegA RegB RegA
        ,Store RegA (Addr 2)            -- h = 4*h = 12
        ,Load (Addr 3) RegA
        ,Push RegA
        ,Load (Addr 2) RegA
        ,Push RegA
        ,Load (Addr 1) RegA
        ,Push RegA
        ,Load (Addr 0) RegA
        ,Push RegA                      -- push known addresses
        ,Const 6 RegA
        ,Compute Add RegA PC RegA
        ,Push RegA                      -- push return address 46
        ,Load (Addr 2) RegA
        ,Push RegA                      -- push arg h=12
        ,Load (Addr 0) RegA
        ,Jump (Ind RegA)                -- jump to TestH
        ,Pop RegA                       -- pop return value
        ,Pop RegB
        ,Store RegB (Addr 0)
        ,Pop RegB
        ,Store RegB (Addr 1)
        ,Pop RegB
        ,Store RegB (Addr 2)
        ,Pop RegB
        ,Store RegB (Addr 3)            -- pop known addresses
        ,Store RegA (Addr 3)            -- n = TestH(h) = 13
        ,Load (Addr 3) RegA
        ,Pop RegB                       -- pop return address
        ,Push RegA                      -- push return value
        ,Jump (Ind RegB)                -- jump to return address
        ,Load (Addr 2) RegA             -- AFTER Test
        ,Push RegA
        ,Load (Addr 1) RegA
        ,Push RegA
        ,Load (Addr 0) RegA
        ,Push RegA                      -- push known addresses
        ,Const 6 RegA
        ,Compute Add RegA PC RegA
        ,Push RegA                      -- push return address 73
        ,Const 3 RegA
        ,Push RegA                      -- push arg 3
        ,Load (Addr 1) RegA
        ,Jump (Ind RegA)                -- jump to Test
        ,Pop RegA                       -- pop return value
        ,Pop RegB
        ,Store RegB (Addr 0)
        ,Pop RegB
        ,Store RegB (Addr 1)
        ,Pop RegB
        ,Store RegB (Addr 2)            -- pop known addresses
        ,Store RegA (Addr 2)            -- k = Test(3) = 13
        ,EndProg]
main = run 1 prog >> putChar '\n'
