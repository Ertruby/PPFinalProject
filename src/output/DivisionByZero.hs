{-# LANGUAGE RecordWildCards #-} 
module Output.DivisionByZero where 
import Sprockell.System
prog:: [Instruction] 
prog = [Const 5 RegA,Store RegA (Addr 0),Const 2 RegA,Store RegA (Addr 1),Const 8 RegA,Store RegA (Addr 2),Const 1 RegA,Store RegA (Addr 3),Const 0 RegA,Store RegA (Addr 4),Const 9 RegA,Store RegA (Addr 5),Const 6 RegA,Store RegA (Addr 6),Const 0 RegA,Store RegA (Addr 12),Compute Add PC Zero RegA,Push RegA,Load (Addr 12) RegA,Load (Addr 6) RegB,Const 1 RegC,Compute Sub RegB RegC RegB,Compute Lt RegA RegB RegA,Const 1 RegB,Compute Xor RegA RegB RegA,Const 23 RegB,Compute Add RegB PC RegB,Branch RegA (Ind RegB),Load (Addr 12) RegA,Load (Addr 12) RegD,Const 0 RegC,Compute Add RegD RegC RegD,Load (Deref RegD) RegC,Load (Addr 12) RegE,Const 1 RegF,Compute Add RegE RegF RegE,Const 0 RegD,Compute Add RegE RegD RegE,Load (Deref RegE) RegD,Compute Div RegC RegD RegC,Const 7 RegB,Compute Add RegA RegB RegA,Store RegC (Deref RegA),Load (Addr 12) RegA,Const 1 RegB,Compute Add RegA RegB RegA,Store RegA (Addr 12),Pop RegA,Jump (Ind RegA),Pop RegA,EndProg]
main = run 1 prog >> putChar '\n'
