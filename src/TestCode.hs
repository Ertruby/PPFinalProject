{-# LANGUAGE RecordWildCards #-}
import Sprockell.System

prog:: [Instruction]
prog = [
        Const 3 RegA
        ,Compute Add RegA PC RegA
        ,Store RegA (Addr 0)                -- store 10 as jump address for task                
        ,Jump (Rel 46)                      -- jump to 55
        ,Pop RegA
        ,Store RegA (Addr 1)                -- store argument n
        ,Const 0 RegA
        ,Store RegA (Addr 2)                -- declare k
        ,Load (Addr 1) RegA
        ,Const 0 RegB                       
        ,Compute Equal RegA RegB RegA       -- n==0
        ,Load (Addr 1) RegB
        ,Const 1 RegC
        ,Compute Equal RegB RegC RegB       -- n==1
        ,Compute Or RegA RegB RegA          -- n==0 || n==1
        ,Const 1 RegB
        ,Compute Xor RegA RegB RegA         -- RegA = 1 if expression is true, otherwise 0
        ,Const 5 RegB
        ,Compute Add RegB PC RegB           
        ,Branch RegA (Ind RegB)             -- jump to 29 if RegA = 1
        ,Const 1 RegA
        ,Store RegA (Addr 2)                -- k = 1
        ,Jump (Rel 23)                      -- jump to 51
        ,Const 8 RegA                       
        ,Compute Add RegA PC RegA
        ,Push RegA                          -- push return address 38
        ,Load (Addr 1) RegA
        ,Const 1 RegB
        ,Compute Sub RegA RegB RegA         -- n - 1
        ,Push RegA                          -- push n - 1
        ,Load (Addr 0) RegA
        ,Jump (Ind RegA)                    -- jump to task: 10
        ,Const 8 RegA                       -- POP WAS HERE
        ,Compute Add RegA PC RegA
        ,Push RegA                          -- push return address 47
        ,Load (Addr 1) RegA
        ,Const 2 RegB
        ,Compute Sub RegA RegB RegA
        ,Push RegA                          -- push n-2
        ,Load (Addr 0) RegA
        ,Jump (Ind RegA)                    -- jump to task: 10
        ,Pop RegB                           -- pop return value n-2
        ,Pop RegA                           -- pop return value n-1
        ,Compute Add RegA RegB RegA
        ,Store RegA (Addr 2)                -- k = fib(n-1) + fib(n-2)
        ,Load (Addr 2) RegA
        ,Pop RegB                           -- pop return address
        ,Push RegA                          -- push return value
        ,Jump (Ind RegB)                    -- jump to return address
        ,Const 6 RegA                       -- after task
        ,Compute Add RegA PC RegA
        ,Push RegA                          -- push return address 62
        ,Const 2 RegA
        ,Push RegA                          -- push argument
        ,Load (Addr 0) RegA
        ,Jump (Ind RegA)                    -- jump to task
        ,Pop RegA
        ,Store RegA (Addr 1)
        ,Nop,Nop,Nop,Nop,Nop,Nop,Nop,Nop,Nop,Nop
        ,EndProg]

prog2:: [Instruction]        
prog2 = [
        Const 3 RegA
        ,Compute Add RegA PC RegA
        ,Store RegA (Addr 0)                -- Task start is 73       
        ,Jump (Rel 33)                      -- skip task: 105
        ,Pop RegA
        ,Store RegA (Addr 1)                -- store arg
        ,Const 0 RegA
        ,Store RegA (Addr 2)                -- k = 0
        ,Load (Addr 1) RegA
        ,Const 1 RegB
        ,Compute Equal RegA RegB RegA       -- n == 1
        ,Const 1 RegB
        ,Compute Xor RegA RegB RegA         -- 
        ,Const 5 RegB
        ,Compute Add RegB PC RegB           -- branch target = 88
        ,Branch RegA (Ind RegB)             -- branch if RegA = 1
        ,Const 1 RegA
        ,Store RegA (Addr 2)                -- k = 1
        ,Jump (Rel 14)                      -- jump to 101
        ,Const 8 RegA                       -- load n stond eerst hier
        ,Compute Add RegA PC RegA
        ,Push RegA                          -- push return address 98
        ,Load (Addr 1) RegA
        ,Const 1 RegB
        ,Compute Sub RegC RegB RegA         -- n - 1
        ,Push RegA                          -- push n-1
        ,Load (Addr 0) RegA
        ,Jump (Ind RegA)                    -- jump to task
        ,Pop RegB                           -- pop return value
        ,Load (Addr 1) RegA                 -- ????
        ,Compute Mul RegA RegB RegA
        ,Store RegA (Addr 2)                -- k = n * Fac(n-1)
        ,Load (Addr 2) RegA                 -- load K
        ,Pop RegB                           -- pop return address
        ,Push RegA                          -- push return value
        ,Jump (Ind RegB)                    -- jump to return address
        ,Const 6 RegA
        ,Compute Add RegA PC RegA
        ,Push RegA                          -- push return address 112
        ,Const 2 RegA
        ,Push RegA                          -- push arg
        ,Load (Addr 0) RegA
        ,Jump (Ind RegA)                    -- jump to task
        ,Pop RegA
        -- ,Store RegA (Addr 1)
        ,Nop,Nop,Nop,Nop,Nop,Nop,Nop,Nop,Nop,Nop
        ,EndProg] 
        
        
prog3 = [
        Const 3 RegA
        ,Compute Add RegA PC RegA
        ,Store RegA (Addr 0)
        ,Jump (Rel 24)
        ,Pop RegA
        ,Store RegA (Addr 1)
        ,Const 0 RegA
        ,Store RegA (Addr 2)
        ,Load (Addr 1) RegA
        ,Const 1 RegB
        ,Compute Equal RegA RegB RegA
        ,Const 1 RegB
        ,Compute Xor RegA RegB RegA
        ,Const 5 RegB
        ,Compute Add RegB PC RegB
        ,Branch RegA (Ind RegB)
        ,Const 1 RegA
        ,Store RegA (Addr 2)
        ,Jump (Rel 5)
        ,Load (Addr 1) RegA
        ,Const 2 RegB
        ,Compute Mul RegA RegB RegA
        ,Store RegA (Addr 2)
        ,Load (Addr 2) RegA
        ,Pop RegB
        ,Push RegA
        ,Jump (Ind RegB)
        ,Const 6 RegA
        ,Compute Add RegA PC RegA
        ,Push RegA
        ,Const 3 RegA
        ,Push RegA
        ,Load (Addr 0) RegA
        ,Jump (Ind RegA)
        ,Pop RegA
        ,Store RegA (Addr 1),Nop,Nop,Nop,Nop,Nop,Nop,Nop,Nop,Nop,Nop
        ,EndProg]
 
 
debug :: SystemState -> String
debug SysState{..}  | (localMem (sprs !! 0) !!! 1) == 3 = "Second shared memaddr equals 3.\n"
                    | (localMem (sprs !! 0) !!! 1) == 1 = "Second shared memaddr equals 1.\n"
                    | (localMem (sprs !! 0) !!! 1) == 2 = "Second shared memaddr equals 2.\n"
                    | (localMem (sprs !! 0) !!! 1) == 4 = "Second shared memaddr equals 4.\n"
                    | (localMem (sprs !! 0) !!! 1) == 5 = "Second shared memaddr equals 5.\n"
                    | (localMem (sprs !! 0) !!! 1) == 6 = "Second shared memaddr equals 6.\n"
                    | (localMem (sprs !! 0) !!! 1) == 0 = "Second shared memaddr equals 0.\n"
                    
                    
                    
                    -- | (localMem (sprs !! 0) !!! 1) == 0 = "Second shared memaddr equals 0.\n"
                    
debug _ = "Nope\n"

main = runDebug debug 1 prog3