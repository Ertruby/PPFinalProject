{-# LANGUAGE RecordWildCards #-}
import Sprockell.System


prog :: [Instruction]
prog = [
        Const 0 RegA
        ,Store RegA (Addr 0)            -- decl
        ,Jump (Rel 11)                  -- jump to after
        ,Pop RegA                       
        ,Store RegA (Addr 3)            -- decl arg
        ,Load (Addr 2) RegA
        ,Const 1 RegB
        ,Compute Add RegA RegB RegA         
        ,Store RegA (Addr 2)            -- incr
        ,Load (Addr 2) RegA             -- give
        ,Pop RegB                       -- pop return address
        ,Push RegA                      -- push return value
        ,Jump (Ind RegB)                -- jump to return address
        ,Const 8 RegA
        ,Compute Add RegA PC RegA       -- compute return address
        ,Push RegA                      -- push return address
        ,Const 3 RegA                   
        ,Push RegA                      -- push first arg
        ,Const 5 RegA
        ,Push RegA                      -- push second arg
        ,Load (Addr 1) RegA
        ,Jump (Ind RegA)                -- jump to task
        ,Nop,Nop,Nop,Nop,Nop,Nop,Nop,Nop,Nop,Nop
        ,EndProg]

        
prog2:: [Instruction]
prog2 = [
        Const 0 RegA
        ,Store RegA (Addr 0)
        ,Jump (Rel 13)
        ,Pop RegA
        ,Store RegA (Addr 2)
        ,Pop RegA
        ,Store RegA (Addr 3)
        ,Load (Addr 2) RegA
        ,Const 1 RegB
        ,Compute Add RegA RegB RegA
        ,Store RegA (Addr 2)
        ,Load (Addr 2) RegA
        ,Pop RegB
        ,Push RegA
        ,Jump (Ind RegB)
        ,Const 8 RegA
        ,Compute Add RegA PC RegA
        ,Push RegA
        ,Const 3 RegA
        ,Push RegA
        ,Const 5 RegA
        ,Push RegA
        ,Load (Addr 1) RegA
        ,Jump (Ind RegA)
        ,Nop,Nop,Nop,Nop,Nop,Nop,Nop,Nop,Nop,Nop
        ,EndProg]
        
fib:: [Instruction]
fib = [
        Jump (Rel 46)                       -- skip task: 110
        ,Pop RegA                           -- begin task
        ,Store RegA (Addr 1)                -- arg n
        ,Const 0 RegA
        ,Store RegA (Addr 2)                -- k = 0            
        ,Load (Addr 1) RegA
        ,Const 0 RegB
        ,Compute Equal RegA RegB RegA       -- n == 0
        ,Load (Addr 1) RegB
        ,Const 1 RegC
        ,Compute Equal RegB RegC RegB       -- n == 1
        ,Compute Or RegA RegB RegA          -- n==0 || n==1
        ,Const 1 RegB
        ,Compute Xor RegA RegB RegA         -- true/false?
        ,Const 5 RegB
        ,Compute Add RegB PC RegB           -- compute start of otherwise: 84
        ,Branch RegA (Ind RegB)             -- jump to otherwise if false
        ,Const 1 RegA
        ,Store RegA (Addr 2)                -- k = 1
        ,Jump (Rel 23)                      -- skip otherwise: 106
        ,Const 8 RegA
        ,Compute Add RegA PC RegA
        ,Push RegA                          -- push return address: 93
        ,Load (Addr 1) RegA
        ,Const 1 RegB
        ,Compute Sub RegA RegB RegA         -- n - 1
        ,Push RegA                          -- push n-1
        ,Load (Addr 0) RegA
        ,Jump (Ind RegA)                    -- jump to task
        ,Pop RegA                           -- pop return value
        ,Const 8 RegA
        ,Compute Add RegA PC RegA
        ,Push RegA                          -- push return address: 103
        ,Load (Addr 1) RegA                 
        ,Const 2 RegB
        ,Compute Sub RegA RegB RegA         -- n - 2    
        ,Push RegA                          -- push n-2
        ,Load (Addr 0) RegA
        ,Jump (Ind RegA)                    -- jump to task
        ,Pop RegB                           -- pop return value
        ,Compute Add RegA RegB RegA         -- add both return values
        ,Store RegA (Addr 2)                -- store result
        ,Load (Addr 2) RegA                 -- end otherwise    
        ,Pop RegB                           -- pop return address
        ,Push RegA                          -- push return value
        ,Jump (Ind RegB)                    -- jump to return address
        ,Const 6 RegA                       -- after task
        ,Compute Add RegA PC RegA
        ,Push RegA                          -- push return address: 117
        ,Const 3 RegA
        ,Push RegA                          -- push arg value: 3
        ,Load (Addr 0) RegA
        ,Jump (Ind RegA)                    -- jump to task: 66
        ,Pop RegA                           -- return address
        ,Store RegA (Addr 1)                -- store return value
        ,Write RegA (Addr 1)
        ,Nop,Nop,Nop,Nop,Nop,Nop,Nop,Nop,Nop,Nop
        ,EndProg]

debug :: SystemState -> String
debug SysState{..}  | (sharedMem !!! 1) == 3 = "Second shared memaddr equals 3.\n"
                    | (sharedMem !!! 1) == 1 = "Second shared memaddr equals 1.\n"
                    | (sharedMem !!! 1) == 2 = "Second shared memaddr equals 2.\n"
                    | (sharedMem !!! 1) == 4 = "Second shared memaddr equals 4.\n"
                    
                    
                    
                    -- | (sharedMem !!! 1) == 0 = "Second shared memaddr equals 0.\n"
                    
debug _ = "Nope\n"

main = runDebug debug 1 fib
