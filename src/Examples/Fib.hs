{-# LANGUAGE RecordWildCards #-} 
module Output.Fib where 
import Sprockell.System
prog:: [Instruction] 
prog = [Const 3 RegA
        ,Compute Add RegA PC RegA
        ,Store RegA (Addr 0)                -- start of Fib is at line 9
        ,Jump (Rel 86)                      -- skip task Fib: line 94
        ,Pop RegA
        ,Store RegA (Addr 1)                -- arg is popped from stack and assigned to n
        ,Const 0 RegA
        ,Store RegA (Addr 2)                -- k = 0
        ,Load (Addr 1) RegA                 -- RegA <-- n
        ,Const 0 RegB                       -- RegB <-- 0
        ,Compute Equal RegA RegB RegA       -- RegA <-- n == 0    
        ,Load (Addr 1) RegB                 -- RegB <-- n
        ,Const 1 RegC                       -- RegC <-- 1
        ,Compute Equal RegB RegC RegB       -- RegB <-- n == 1
        ,Compute Or RegA RegB RegA          -- RegA <-- n == 0 || n == 1
        ,Const 1 RegB
        ,Compute Xor RegA RegB RegA         -- RegA <-- !(n == 0 || n == 1)
        ,Const 5 RegB
        ,Compute Add RegB PC RegB           -- RegB <-- instruction line 28 (otherwise)
        ,Branch RegA (Ind RegB)             -- go to otherwise if the expression is false
        ,Const 1 RegA
        ,Store RegA (Addr 2)                -- k = 1
        ,Jump (Rel 63)                      -- skip otherwise
        ,Load (Addr 3) RegA
        ,Push RegA
        ,Load (Addr 2) RegA
        ,Push RegA
        ,Load (Addr 1) RegA
        ,Push RegA
        ,Load (Addr 0) RegA
        ,Push RegA                          -- push known addresses
        ,Const 8 RegA
        ,Compute Add RegA PC RegA
        ,Push RegA                          -- push return address: 45
        ,Load (Addr 1) RegA
        ,Const 1 RegB
        ,Compute Sub RegA RegB RegA
        ,Push RegA                          -- push argument n-1
        ,Load (Addr 0) RegA
        ,Jump (Ind RegA)                    -- jump to task Fib
        ,Pop RegA                           -- RegA <-- return value
        ,Pop RegB
        ,Store RegB (Addr 0)
        ,Pop RegB
        ,Store RegB (Addr 1)
        ,Pop RegB
        ,Store RegB (Addr 2)
        ,Pop RegB
        ,Store RegB (Addr 3)                -- pop known addresses and restore them
        ,Store RegA (Addr 3)                -- l = return value
        ,Load (Addr 4) RegA
        ,Push RegA
        ,Load (Addr 3) RegA
        ,Push RegA
        ,Load (Addr 2) RegA
        ,Push RegA
        ,Load (Addr 1) RegA
        ,Push RegA
        ,Load (Addr 0) RegA
        ,Push RegA                          -- push known addresses
        ,Const 8 RegA
        ,Compute Add RegA PC RegA
        ,Push RegA
        ,Load (Addr 1) RegA
        ,Const 2 RegB
        ,Compute Sub RegA RegB RegA
        ,Push RegA
        ,Load (Addr 0) RegA
        ,Jump (Ind RegA)
        ,Pop RegA
        ,Pop RegB
        ,Store RegB (Addr 0)
        ,Pop RegB
        ,Store RegB (Addr 1)
        ,Pop RegB
        ,Store RegB (Addr 2)
        ,Pop RegB
        ,Store RegB (Addr 3)
        ,Pop RegB
        ,Store RegB (Addr 4)
        ,Store RegA (Addr 4)
        ,Load (Addr 3) RegA
        ,Load (Addr 4) RegB
        ,Compute Add RegA RegB RegA
        ,Store RegA (Addr 2)
        ,Load (Addr 2) RegA
        ,Pop RegB
        ,Push RegA
        ,Jump (Ind RegB)
        ,Load (Addr 1) RegA                 -- AFTER task Fib
        ,Push RegA
        ,Load (Addr 0) RegA
        ,Push RegA                          -- push all known addresses
        ,Const 6 RegA
        ,Compute Add RegA PC RegA
        ,Push RegA                          -- push return address line 105
        ,Const 4 RegA
        ,Push RegA                          -- push argument 4
        ,Load (Addr 0) RegA
        ,Jump (Ind RegA)                    -- jump to task Fib
        ,Pop RegA
        ,Pop RegB
        ,Store RegB (Addr 0)
        ,Pop RegB
        ,Store RegB (Addr 1)
        ,Store RegA (Addr 1)
        ,EndProg]
main = run 1 prog >> putChar '\n'
