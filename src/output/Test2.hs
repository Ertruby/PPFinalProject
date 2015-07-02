{-# LANGUAGE RecordWildCards #-} 
module Output.Test2 where 
import Sprockell.System
prog:: [Instruction] 
prog = [Const 1 RegA
        ,Store RegA (Addr 0)
        ,Const 2 RegA
        ,Store RegA (Addr 1)
        ,Const 3 RegA
        ,Store RegA (Addr 2)
        ,Const 0 RegA
        ,Const 1 RegC
        ,Const 0 RegB
        ,Compute Add RegA RegB RegA
        ,Store RegC (Deref RegA)
        ,EndProg]
main = run 1 prog >> putChar '\n'
