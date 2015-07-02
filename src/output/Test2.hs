{-# LANGUAGE RecordWildCards #-} 
module Output.Test2 where 
import Sprockell.System
prog:: [Instruction] 
prog = [Const 3 RegA,Compute Add RegA PC RegA,Store RegA (Addr 0),Jump (Rel 38),Pop RegA,Store RegA (Addr 1),Const 1 RegA,Store RegA (Addr 4),Const 2 RegA,Store RegA (Addr 3),Const 3 RegA,Store RegA (Addr 2),Const 3 RegA,Store RegA (Addr 5),Const 1 RegA,Store RegA (Addr 10),Const 2 RegA,Store RegA (Addr 9),Const 3 RegA,Store RegA (Addr 8),Const 4 RegA,Store RegA (Addr 7),Const 5 RegA,Store RegA (Addr 6),Load (Addr 5) RegA,Const 8 RegC,Const 6 RegB,Compute Add RegA RegB RegA,Store RegC (Deref RegA),Load (Addr 5) RegB,Const 6 RegA,Compute Add RegB RegA RegB,Load (Deref RegB) RegA,Const 4 RegC,Const 6 RegB,Compute Add RegC RegB RegC,Load (Deref RegC) RegB,Compute Add RegA RegB RegA,Store RegA (Addr 5),Pop RegB,Jump (Ind RegB),Load (Addr 1) RegA,Push RegA,Load (Addr 0) RegA,Push RegA,Const 6 RegA,Compute Add RegA PC RegA,Push RegA,Const 3 RegA,Push RegA,Load (Addr 0) RegA,Jump (Ind RegA),Pop RegA,Store RegA (Addr 1),Pop RegA,Store RegA (Addr 0),EndProg]
main = run 1 prog >> putChar '\n'
