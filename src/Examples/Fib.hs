{-# LANGUAGE RecordWildCards #-} 
module Output.Fib where 
import Sprockell.System
prog:: [Instruction] 
prog = [Const 3 RegA
        ,Compute Add RegA PC RegA
        ,Store RegA (Addr 0)
        ,Jump (Rel 86)
        ,Pop RegA
        ,Store RegA (Addr 1)
        ,Const 0 RegA
        ,Store RegA (Addr 2)
        ,Load (Addr 1) RegA
        ,Const 0 RegB
        ,Compute Equal RegA RegB RegA
        ,Load (Addr 1) RegB
        ,Const 1 RegC
        ,Compute Equal RegB RegC RegB
        ,Compute Or RegA RegB RegA
        ,Const 1 RegB
        ,Compute Xor RegA RegB RegA
        ,Const 5 RegB
        ,Compute Add RegB PC RegB
        ,Branch RegA (Ind RegB)
        ,Const 1 RegA
        ,Store RegA (Addr 2)
        ,Jump (Rel 63)
        ,Load (Addr 3) RegA
        ,Push RegA
        ,Load (Addr 2) RegA
        ,Push RegA
        ,Load (Addr 1) RegA
        ,Push RegA
        ,Load (Addr 0) RegA
        ,Push RegA
        ,Const 8 RegA
        ,Compute Add RegA PC RegA
        ,Push RegA
        ,Load (Addr 1) RegA
        ,Const 1 RegB
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
        ,Store RegA (Addr 3)
        ,Load (Addr 4) RegA
        ,Push RegA
        ,Load (Addr 3) RegA
        ,Push RegA
        ,Load (Addr 2) RegA
        ,Push RegA
        ,Load (Addr 1) RegA
        ,Push RegA
        ,Load (Addr 0) RegA
        ,Push RegA
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
        ,Load (Addr 1) RegA
        ,Push RegA
        ,Load (Addr 0) RegA
        ,Push RegA
        ,Const 6 RegA
        ,Compute Add RegA PC RegA
        ,Push RegA
        ,Const 4 RegA
        ,Push RegA
        ,Load (Addr 0) RegA
        ,Jump (Ind RegA)
        ,Pop RegA
        ,Pop RegB
        ,Store RegB (Addr 0)
        ,Pop RegB
        ,Store RegB (Addr 1)
        ,Store RegA (Addr 1)
        ,EndProg]
main = run 1 prog >> putChar '\n'
