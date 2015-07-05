{-# LANGUAGE RecordWildCards #-} 
module Output.Test4.txt where 
import Sprockell.System
prog:: [Instruction] 
prog = [Const 3 RegA,Compute Add RegA PC RegA,Store RegA (Addr 0),Jump (Rel 11),Pop RegA,Store RegA (Addr 1),Pop RegA,Store RegA (Addr 2),Load (Addr 1) RegA,Const 1 RegB,Compute Add RegA RegB RegA,Store RegA (Addr 3),Pop RegB,Jump (Ind RegB),Const 0 RegA,Store RegA (Addr 1),Load (Addr 2) RegA,Push RegA,Load (Addr 1) RegA,Push RegA,Load (Addr 0) RegA,Push RegA,Const 8 RegA,Compute Add RegA PC RegA,Push RegA,Const 5 RegA,Push RegA,Const 5 RegA,Push RegA,Load (Addr 0) RegA,Jump (Ind RegA),Pop RegA,Store RegA (Addr 0),Pop RegA,Store RegA (Addr 1),Pop RegA,Store RegA (Addr 2),EndProg]
main = run 1 prog >> putChar '\n'
