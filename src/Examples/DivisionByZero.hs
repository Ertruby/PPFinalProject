{-# LANGUAGE RecordWildCards #-} 
module Output.DivisionByZero where 
import Sprockell.System
prog:: [Instruction] 
prog = [Const 5 RegA
        ,Store RegA (Addr 0)
        ,Const 2 RegA
        ,Store RegA (Addr 1)
        ,Const 8 RegA
        ,Store RegA (Addr 2)
        ,Const 1 RegA
        ,Store RegA (Addr 3)
        ,Const 0 RegA
        ,Store RegA (Addr 4)
        ,Const 9 RegA
        ,Store RegA (Addr 5)            -- Addr 0-5 = k = [5,2,8,1,0,9]
        ,Const 6 RegA
        ,Store RegA (Addr 6)            -- l = 6
        ,Const 0 RegA
        ,Store RegA (Addr 12)           -- i = 0, Addr 7-11 = b
        ,Compute Add PC Zero RegA
        ,Push RegA                      -- push line 21 as start of while
        ,Load (Addr 12) RegA            -- RegA <-- i
        ,Load (Addr 6) RegB             -- RegB <-- l
        ,Const 1 RegC                   -- RegC <-- 1
        ,Compute Sub RegB RegC RegB     -- RegB <-- l - 1
        ,Compute Lt RegA RegB RegA      -- RegA <-- i < l - 1
        ,Const 1 RegB                   -- RegB <-- 1
        ,Compute Xor RegA RegB RegA     -- Invert RegA    
        ,Const 23 RegB
        ,Compute Add RegB PC RegB       -- line 54 is end of while
        ,Branch RegA (Ind RegB)         -- jump to end if RegA > 0
        ,Load (Addr 12) RegA            -- RegA <-- i
        ,Load (Addr 12) RegD            -- RegD <-- i
        ,Const 0 RegC                   -- RegC <-- 0 //begin of k
        ,Compute Add RegD RegC RegD     -- RegD <-- i+0
        ,Load (Deref RegD) RegC         -- RegC <-- k[i]
        ,Load (Addr 12) RegE            -- RegE <-- i
        ,Const 1 RegF                   -- RegF <-- 1
        ,Compute Add RegE RegF RegE     -- RegE <-- i+1
        ,Const 0 RegD                   -- RegD <-- 0 //begin of k
        ,Compute Add RegE RegD RegE     -- RegE <-- i+1+0
        ,Load (Deref RegE) RegD         -- RegD <-- k[i+1]
        ,Compute Div RegC RegD RegC     -- RegC <-- k[i]/k[i+1]
        ,Const 7 RegB                   -- RegB <-- 7 //begin of b
        ,Compute Add RegA RegB RegA     -- RegA <-- i + 7 = b[i]
        ,Store RegC (Deref RegA)        -- b[i] = k[i]/k[i+1]
        ,Load (Addr 12) RegA            -- RegA <-- i
        ,Const 1 RegB                   -- RegB <-- 1
        ,Compute Add RegA RegB RegA     -- RegA <-- i + 1    
        ,Store RegA (Addr 12)           -- i = i+1: increment i
        ,Pop RegA                       -- pop start of while
        ,Jump (Ind RegA)                -- jump to start of while
        ,Pop RegA                       -- cleanup: pop start of while from stack
        ,EndProg]
main = run 1 prog >> putChar '\n'

