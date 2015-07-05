{-# LANGUAGE RecordWildCards #-} 
module Output.InfiniteLoop where 
import Sprockell.System
prog:: [Instruction] 
prog = [Const 0 RegA
        ,Store RegA (Addr 0)            -- b
        ,Compute Add PC Zero RegA
        ,Push RegA                      -- push line 7 as start of while
        ,Const 1 RegA                   -- RegA <-- 1
        ,Const 1 RegB                   -- RegB <-- 1
        ,Compute Xor RegA RegB RegA     -- RegA <-- 0
        ,Const 8 RegB
        ,Compute Add RegB PC RegB       -- compute instruction after while
        ,Branch RegA (Ind RegB)         -- jump to after while if RegA /= 0 (never happens)
        ,Load (Addr 0) RegA             -- RegA <-- b
        ,Const 1 RegB                   -- RegB <-- 1
        ,Compute Add RegA RegB RegA     -- RegA <-- b+1
        ,Store RegA (Addr 0)            -- b = b+1
        ,Pop RegA                       -- pop start of while
        ,Jump (Ind RegA)                -- jump to star of while
        ,Pop RegA                       -- cleanup: pop start of while
        ,EndProg]
main = run 1 prog >> putChar '\n'
