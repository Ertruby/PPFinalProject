{-# LANGUAGE RecordWildCards #-}
import Sprockell.System



prog :: [Instruction]
prog = [
        Const 0 RegA
        ,Store RegA (Addr 0)
        ,Const 0 RegA
        ,Store RegA (Addr 1)
        ,Compute Add PC Zero RegA       -- jump target
        ,Push RegA
        ,Load (Addr 1) RegA
        ,Const 7 RegB
        ,Compute Lt RegA RegB RegA
        ,Const 1 RegB
        ,Compute Xor RegA RegB RegA
        ,Const 9 RegB
        ,Compute Add RegB PC RegB
        ,Branch RegA (Ind RegB)
        ,Load (Addr 1) RegA
        ,Const 1 RegB
        ,Compute Add RegA RegB RegA
        ,Store RegA (Addr 1)
        ,Write RegA (Addr 1)
        ,Pop RegA
        ,Jump (Ind RegA)
        ,Nop,Nop,Nop,Nop,Nop,Nop,Nop,Nop,Nop,Nop
        ,EndProg]


debug :: SystemState -> String
debug SysState{..}  | (sharedMem !!! 1) == 7 = "Second shared memaddr equals 7.\n"
                    
                    
                    
                    -- | (sharedMem !!! 1) == 0 = "Second shared memaddr equals 0.\n"
                    
debug _ = "Nope\n"

main = runDebug debug 1 prog
