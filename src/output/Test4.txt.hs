{-# LANGUAGE RecordWildCards #-} 
module Output.Test4.txt where 
import Sprockell.System
prog:: [Instruction] 
prog = [Const 1 RegA,Store RegA (Addr 0),Const 2 RegA,Store RegA (Addr 1),Const 3 RegA,Store RegA (Addr 2),EndProg]
main = run 1 prog >> putChar '\n'
