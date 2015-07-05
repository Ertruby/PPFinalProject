{-# LANGUAGE RecordWildCards #-} 
module Output.Test4 where 
import Sprockell.System
prog:: [Instruction] 
prog = [Const 5 RegA
        ,Const 6 RegB
        ,Compute NEq RegA RegB RegA
        ,Const 1 RegB
        ,Compute Xor RegA RegB RegA
        ,Const 3 RegB
        ,Compute Add RegB PC RegB
        ,Branch RegA (Ind RegB)
        ,Jump (Rel 1)
        ,EndProg]
main = run 1 prog >> putChar '\n'
