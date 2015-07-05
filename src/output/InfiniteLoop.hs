{-# LANGUAGE RecordWildCards #-} 
module Output.InfiniteLoop where 
import Sprockell.System
prog:: [Instruction] 
prog = [Const 0 RegA,Store RegA (Addr 0),Compute Add PC Zero RegA,Push RegA,Const 1 RegA,Const 1 RegB,Compute Xor RegA RegB RegA,Const 8 RegB,Compute Add RegB PC RegB,Branch RegA (Ind RegB),Load (Addr 0) RegA,Const 1 RegB,Compute Add RegA RegB RegA,Store RegA (Addr 0),Pop RegA,Jump (Ind RegA),Pop RegA,EndProg]
main = run 1 prog >> putChar '\n'
