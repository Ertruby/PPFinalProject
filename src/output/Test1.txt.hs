{-# LANGUAGE RecordWildCards #-} 
module Output.Test1.txt where 
import Sprockell.System
prog:: [Instruction] 
prog = [Const 5 RegA,Store RegA (Addr 3),Const 1 RegA,Store RegA (Addr 0),Const (-2) RegA,Store RegA (Addr 1),Load (Addr 3) RegA,Store RegA (Addr 2),Const 3 RegA,Compute Add RegA PC RegA,Store RegA (Addr 4),Jump (Rel 11),Pop RegA,Store RegA (Addr 5),Load (Addr 5) RegA,Const 1 RegB,Compute Add RegA RegB RegA,Store RegA (Addr 6),Load (Addr 6) RegA,Pop RegB,Push RegA,Jump (Ind RegB),Const 3 RegA,Compute Add RegA PC RegA,Store RegA (Addr 5),Jump (Rel 194),Pop RegA,Store RegA (Addr 6),Pop RegA,Store RegA (Addr 7),Pop RegA,Store RegA (Addr 8),Const 10 RegA,Store RegA (Addr 9),Load (Addr 7) RegA,Load (Addr 8) RegB,Compute Add RegA RegB RegA,Store RegA (Addr 15),Const 1 RegA,Store RegA (Addr 16),Const 2 RegA,Store RegA (Addr 17),Const 3 RegA,Store RegA (Addr 18),Const 1 RegA,Const 2 RegC,Const 10 RegB,Compute Add RegA RegB RegA,Store RegC (Deref RegA),Const 1 RegA,Load (Addr 15) RegC,Const 10 RegB,Compute Add RegA RegB RegA,Store RegC (Deref RegA),Const 3 RegA,Compute Add RegA PC RegA,Store RegA (Addr 19),Jump (Rel 10),Pop RegA,Store RegA (Addr 20),Const 1 RegA,Const 9 RegC,Const 10 RegB,Compute Add RegA RegB RegA,Store RegC (Deref RegA),Pop RegB,Jump (Ind RegB),Load (Addr 20) RegA,Push RegA,Load (Addr 19) RegA,Push RegA,Load (Addr 18) RegA,Push RegA,Load (Addr 17) RegA,Push RegA,Load (Addr 16) RegA,Push RegA,Load (Addr 15) RegA,Push RegA,Load (Addr 14) RegA,Push RegA,Load (Addr 13) RegA,Push RegA,Load (Addr 12) RegA,Push RegA,Load (Addr 11) RegA,Push RegA,Load (Addr 10) RegA,Push RegA,Load (Addr 9) RegA,Push RegA,Load (Addr 8) RegA,Push RegA,Load (Addr 7) RegA,Push RegA,Load (Addr 6) RegA,Push RegA,Load (Addr 5) RegA,Push RegA,Load (Addr 4) RegA,Push RegA,Load (Addr 3) RegA,Push RegA,Load (Addr 2) RegA,Push RegA,Load (Addr 1) RegA,Push RegA,Load (Addr 0) RegA,Push RegA,Const 6 RegA,Compute Add RegA PC RegA,Push RegA,Const 5 RegA,Push RegA,Load (Addr 19) RegA,Jump (Ind RegA),Pop RegA,Store RegA (Addr 0),Pop RegA,Store RegA (Addr 1),Pop RegA,Store RegA (Addr 2),Pop RegA,Store RegA (Addr 3),Pop RegA,Store RegA (Addr 4),Pop RegA,Store RegA (Addr 5),Pop RegA,Store RegA (Addr 6),Pop RegA,Store RegA (Addr 7),Pop RegA,Store RegA (Addr 8),Pop RegA,Store RegA (Addr 9),Pop RegA,Store RegA (Addr 10),Pop RegA,Store RegA (Addr 11),Pop RegA,Store RegA (Addr 12),Pop RegA,Store RegA (Addr 13),Pop RegA,Store RegA (Addr 14),Pop RegA,Store RegA (Addr 15),Pop RegA,Store RegA (Addr 16),Pop RegA,Store RegA (Addr 17),Pop RegA,Store RegA (Addr 18),Pop RegA,Store RegA (Addr 19),Pop RegA,Store RegA (Addr 20),Const 0 RegA,Store RegA (Addr 6),Const 1 RegA,Store RegA (Addr 20),Compute Add PC Zero RegA,Push RegA,Load (Addr 20) RegA,Const 1 RegB,Compute Xor RegA RegB RegA,Const 19 RegB,Compute Add RegB PC RegB,Branch RegA (Ind RegB),Load (Addr 9) RegA,Const 1 RegB,Compute Add RegA RegB RegA,Store RegA (Addr 9),Load (Addr 9) RegA,Const 20 RegB,Compute Gt RegA RegB RegA,Const 1 RegB,Compute Xor RegA RegB RegA,Const 5 RegB,Compute Add RegB PC RegB,Branch RegA (Ind RegB),Const 0 RegA,Store RegA (Addr 20),Jump (Rel 1),Pop RegA,Jump (Ind RegA),Pop RegA,Load (Addr 6) RegA,Const 0 RegB,Compute Equal RegA RegB RegA,Const 1 RegB,Compute Xor RegA RegB RegA,Const 16 RegB,Compute Add RegB PC RegB,Branch RegA (Ind RegB),Const 2 RegA,Const 0 RegD,Const 10 RegC,Compute Add RegD RegC RegD,Load (Deref RegD) RegC,Const 1 RegE,Const 10 RegD,Compute Add RegE RegD RegE,Load (Deref RegE) RegD,Compute Mul RegC RegD RegC,Const 10 RegB,Compute Add RegA RegB RegA,Store RegC (Deref RegA),Jump (Rel 6),Const 2 RegA,Load (Addr 9) RegC,Const 10 RegB,Compute Add RegA RegB RegA,Store RegC (Deref RegA),Load (Addr 9) RegA,Pop RegB,Push RegA,Jump (Ind RegB),Load (Addr 6) RegA,Push RegA,Load (Addr 5) RegA,Push RegA,Load (Addr 4) RegA,Push RegA,Load (Addr 3) RegA,Push RegA,Load (Addr 2) RegA,Push RegA,Load (Addr 1) RegA,Push RegA,Load (Addr 0) RegA,Push RegA,Const 6 RegA,Compute Add RegA PC RegA,Push RegA,Load (Addr 3) RegA,Push RegA,Load (Addr 4) RegA,Jump (Ind RegA),Pop RegA,Pop RegB,Store RegB (Addr 0),Pop RegB,Store RegB (Addr 1),Pop RegB,Store RegB (Addr 2),Pop RegB,Store RegB (Addr 3),Pop RegB,Store RegB (Addr 4),Pop RegB,Store RegB (Addr 5),Pop RegB,Store RegB (Addr 6),Store RegA (Addr 6),EndProg]
main = run 1 prog >> putChar '\n'