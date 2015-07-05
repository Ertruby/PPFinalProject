{-# LANGUAGE RecordWildCards #-} 
module Output.TypeCheckFail.txt where 
import Sprockell.System
prog:: [Instruction] 
prog = [Const 3 RegA,Compute Add RegA PC RegA,Store RegA (Addr 0),Jump (Rel 7),Pop RegA,Store RegA (Addr 1),Const 0 RegA,Store RegA (Addr 2),Pop RegB,Jump (Ind RegB),Load (Addr 1) RegA,Push RegA,Load (Addr 0) RegA,Push RegA,Const 6 RegA,Compute Add RegA PC RegA,Push RegA,Const 5 RegA,Push RegA,Load (Addr 0) RegA,Jump (Ind RegA),Pop RegA,Store RegA (Addr 0),Pop RegA,Store RegA (Addr 1),Const 4 RegA,Store RegA (Addr 1),EndProg]
main = run 1 prog >> putChar '\n'
