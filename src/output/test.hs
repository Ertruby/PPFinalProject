{-# LANGUAGE RecordWildCards #-} 
module Output.Test where 
import Sprockell.System
prog:: [Instruction] 
prog = [Const 3 RegA,Compute Add RegA PC RegA,Store RegA (Addr 0),Jump (Rel 21),Pop RegA,Store RegA (Addr 1),Const 1 RegA,Store RegA (Addr 4),Const 2 RegA,Store RegA (Addr 3),Const 3 RegA,Store RegA (Addr 2),Const 1 RegA,Store RegA (Addr 9),Const 2 RegA,Store RegA (Addr 8),Const 3 RegA,Store RegA (Addr 7),Const 4 RegA,Store RegA (Addr 6),Const 5 RegA,Store RegA (Addr 5),Pop RegB,Jump (Ind RegB),Load (Addr 1) RegA,Push RegA,Load (Addr 0) RegA,Push RegA,Const 6 RegA,Compute Add RegA PC RegA,Push RegA,Const 3 RegA,Push RegA,Load (Addr 0) RegA,Jump (Ind RegA),Pop RegA,Store RegA (Addr 1),Pop RegA,Store RegA (Addr 0),EndProg]
main = run 1 prog >> putChar '\n'
