{-# LANGUAGE RecordWildCards #-}
import Sprockell.System

prog:: [Instruction] 
prog = [Const 5 RegA
        ,Store RegA (Addr 3)            -- herman = 5
        ,Const 1 RegA
        ,Store RegA (Addr 0)
        ,Const (-2) RegA
        ,Store RegA (Addr 1)
        ,Load (Addr 3) RegA
        ,Store RegA (Addr 2)            -- a is [1,-2,herman]
        ,Const 3 RegA
        ,Compute Add RegA PC RegA
        ,Store RegA (Addr 4)            -- start of task H is 17
        ,Jump (Rel 11)                  -- skip task H: 27
        ,Pop RegA
        ,Store RegA (Addr 5)            -- i = herman
        ,Load (Addr 5) RegA
        ,Const 1 RegB
        ,Compute Add RegA RegB RegA
        ,Store RegA (Addr 6)            -- n = i + 1
        ,Load (Addr 6) RegA
        ,Pop RegB                       -- pop return address
        ,Push RegA                      -- push return value
        ,Jump (Ind RegB)                -- jump to return address
        ,Const 3 RegA                   -- AFTER task H
        ,Compute Add RegA PC RegA
        ,Store RegA (Addr 5)            -- start of task Func is 31
        ,Jump (Rel 200)                 -- skip task Func: 230
        ,Pop RegA
        ,Store RegA (Addr 6)            -- g
        ,Pop RegA
        ,Store RegA (Addr 7)            -- i
        ,Pop RegA
        ,Store RegA (Addr 8)            -- j
        ,Const 10 RegA
        ,Store RegA (Addr 9)            -- b = 10
        ,Load (Addr 7) RegA
        ,Load (Addr 8) RegB
        ,Compute Add RegA RegB RegA
        ,Store RegA (Addr 15)           -- c = i + j
        ,Const 1 RegA
        ,Store RegA (Addr 16)
        ,Const 2 RegA
        ,Store RegA (Addr 17)
        ,Const 3 RegA
        ,Store RegA (Addr 18)           -- o = [1,2,3]
        ,Const 0 RegA
        ,Store RegA (Addr 19)           -- k = 0    // deleted
        ,Const 1 RegA
        ,Const 2 RegC
        ,Const 10 RegB
        ,Compute Add RegA RegB RegA
        ,Store RegC (Deref RegA)        -- (Addr 11) == a[1] = 2
        ,Const 1 RegA
        ,Load (Addr 15) RegC
        ,Const 10 RegB
        ,Compute Add RegA RegB RegA
        ,Store RegC (Deref RegA)        -- a[1] = c    
        ,Const 3 RegA
        ,Compute Add RegA PC RegA
        ,Store RegA (Addr 20)           -- start of task Stuff is 65
        ,Jump (Rel 10)                  -- skip task Stuff: 74
        ,Pop RegA
        ,Store RegA (Addr 21)           -- i
        ,Const 1 RegA
        ,Const 9 RegC
        ,Const 10 RegB
        ,Compute Add RegA RegB RegA
        ,Store RegC (Deref RegA)        -- a[1] = 9
        ,Pop RegB                       -- pop return address
        ,Jump (Ind RegB)                -- jump to return address
        ,Load (Addr 21) RegA            -- AFTER task Stuff
        ,Push RegA
        ,Load (Addr 20) RegA
        ,Push RegA
        ,Load (Addr 19) RegA
        ,Push RegA
        ,Load (Addr 18) RegA
        ,Push RegA
        ,Load (Addr 17) RegA
        ,Push RegA
        ,Load (Addr 16) RegA
        ,Push RegA
        ,Load (Addr 15) RegA
        ,Push RegA
        ,Load (Addr 14) RegA
        ,Push RegA
        ,Load (Addr 13) RegA
        ,Push RegA
        ,Load (Addr 12) RegA
        ,Push RegA
        ,Load (Addr 11) RegA
        ,Push RegA
        ,Load (Addr 10) RegA
        ,Push RegA
        ,Load (Addr 9) RegA
        ,Push RegA
        ,Load (Addr 8) RegA
        ,Push RegA
        ,Load (Addr 7) RegA
        ,Push RegA
        ,Load (Addr 6) RegA
        ,Push RegA
        ,Load (Addr 5) RegA
        ,Push RegA
        ,Load (Addr 4) RegA
        ,Push RegA
        ,Load (Addr 3) RegA
        ,Push RegA
        ,Load (Addr 2) RegA
        ,Push RegA
        ,Load (Addr 1) RegA
        ,Push RegA
        ,Load (Addr 0) RegA
        ,Push RegA                      -- push known addresses
        ,Const 6 RegA
        ,Compute Add RegA PC RegA
        ,Push RegA                      -- push return address 125
        ,Const 5 RegA
        ,Push RegA                      -- push arg 5
        ,Load (Addr 20) RegA
        ,Jump (Ind RegA)                -- jump to task Stuff
        ,Pop RegA                      
        ,Store RegA (Addr 21)
        ,Pop RegA
        ,Store RegA (Addr 20)
        ,Pop RegA
        ,Store RegA (Addr 19)
        ,Pop RegA
        ,Store RegA (Addr 18)
        ,Pop RegA
        ,Store RegA (Addr 17)
        ,Pop RegA
        ,Store RegA (Addr 16)
        ,Pop RegA
        ,Store RegA (Addr 15)
        ,Pop RegA
        ,Store RegA (Addr 14)
        ,Pop RegA
        ,Store RegA (Addr 13)
        ,Pop RegA
        ,Store RegA (Addr 12)
        ,Pop RegA
        ,Store RegA (Addr 11)
        ,Pop RegA
        ,Store RegA (Addr 10)
        ,Pop RegA
        ,Store RegA (Addr 9)
        ,Pop RegA
        ,Store RegA (Addr 8)
        ,Pop RegA
        ,Store RegA (Addr 7)
        ,Pop RegA
        ,Store RegA (Addr 6)
        ,Pop RegA
        ,Store RegA (Addr 5)
        ,Pop RegA
        ,Store RegA (Addr 4)
        ,Pop RegA
        ,Store RegA (Addr 3)
        ,Pop RegA
        ,Store RegA (Addr 2)
        ,Pop RegA
        ,Store RegA (Addr 1)
        ,Pop RegA                       
        ,Store RegA (Addr 0)            -- pop known addresses
        ,Const 0 RegA           
        ,Store RegA (Addr 6)            -- g = false
        ,Const 1 RegA
        ,Store RegA (Addr 21)           -- decl h = true
        ,Compute Add PC Zero RegA
        ,Push RegA                      -- push start of while 173
        ,Load (Addr 21) RegA            -- load h
        ,Const 1 RegB
        ,Compute Xor RegA RegB RegA     -- RegA = not h
        ,Const 19 RegB
        ,Compute Add RegB PC RegB
        ,Branch RegA (Ind RegB)         -- if h is false, jump to 198
        ,Load (Addr 9) RegA
        ,Const 1 RegB
        ,Compute Add RegA RegB RegA
        ,Store RegA (Addr 9)            -- increment b
        ,Load (Addr 9) RegA
        ,Const 20 RegB
        ,Compute Gt RegA RegB RegA
        ,Const 1 RegB
        ,Compute Xor RegA RegB RegA
        ,Const 5 RegB
        ,Compute Add RegB PC RegB
        ,Branch RegA (Ind RegB)         -- if b>20 is false, jump to 196
        ,Const 0 RegA
        ,Store RegA (Addr 21)           -- h = false
        ,Jump (Rel 1)
        ,Pop RegA
        ,Jump (Ind RegA)                -- jump to begin while
        ,Pop RegA                       -- AFTER while
        ,Load (Addr 6) RegA
        ,Const 0 RegB
        ,Compute Equal RegA RegB RegA   
        ,Const 1 RegB
        ,Compute Xor RegA RegB RegA
        ,Const 16 RegB
        ,Compute Add RegB PC RegB
        ,Branch RegA (Ind RegB)         -- if g is false, jump to 221
        ,Const 2 RegA
        ,Const 0 RegD
        ,Const 10 RegC
        ,Compute Add RegD RegC RegD     
        ,Load (Deref RegD) RegC         -- RegC = a[0] (Addr 10)
        ,Const 1 RegE
        ,Const 10 RegD
        ,Compute Add RegE RegD RegE
        ,Load (Deref RegE) RegD         -- RegD = a[1] (Addr 11)
        ,Compute Mul RegC RegD RegC
        ,Const 10 RegB
        ,Compute Add RegA RegB RegA
        ,Store RegC (Deref RegA)        -- a[2] = a[0] * a[1] (Addr 12)   
        ,Jump (Rel 6)                   -- skip otherwise: 226
        ,Const 2 RegA
        ,Load (Addr 9) RegC
        ,Const 10 RegB
        ,Compute Add RegA RegB RegA
        ,Store RegC (Deref RegA)        -- a[2] = b (Addr 12)
        ,Load (Addr 9) RegA             -- load b
        ,Pop RegB                       -- pop return address
        ,Push RegA                      -- push b
        ,Jump (Ind RegB)
        ,Load (Addr 6) RegA                 -- AFTER task Func
        ,Push RegA
        ,Load (Addr 5) RegA
        ,Push RegA
        ,Load (Addr 4) RegA
        ,Push RegA
        ,Load (Addr 3) RegA
        ,Push RegA
        ,Load (Addr 2) RegA
        ,Push RegA
        ,Load (Addr 1) RegA
        ,Push RegA
        ,Load (Addr 0) RegA
        ,Push RegA                          -- push known addresses    
        ,Const 6 RegA
        ,Compute Add RegA PC RegA
        ,Push RegA                          -- push return address 251
        ,Load (Addr 3) RegA
        ,Push RegA                          -- push arg herman
        ,Load (Addr 4) RegA
        ,Jump (Ind RegA)                    -- jump to task H
        ,Pop RegA                           -- pop return value
        ,Pop RegB
        ,Store RegB (Addr 6)
        ,Pop RegB
        ,Store RegB (Addr 5)
        ,Pop RegB
        ,Store RegB (Addr 4)
        ,Pop RegB
        ,Store RegB (Addr 3)
        ,Pop RegB
        ,Store RegB (Addr 2)
        ,Pop RegB
        ,Store RegB (Addr 1)
        ,Pop RegB
        ,Store RegB (Addr 0)                -- pop known addresses
        ,Store RegA (Addr 6)                -- c = H(herman)
        ,EndProg]
        
prog1:: [Instruction] 
prog1 = [Const 3 RegA
        ,Compute Add RegA PC RegA
        ,Store RegA (Addr 0)            -- task TestH starts at 9
        ,Jump (Rel 13)                  -- skip TestH: 21
        ,Pop RegA
        ,Store RegA (Addr 1)            -- n = 12
        ,Load (Addr 1) RegA
        ,Store RegA (Addr 2)            -- a = n = 12
        ,Load (Addr 2) RegA
        ,Const 1 RegB
        ,Compute Add RegA RegB RegA
        ,Store RegA (Addr 2)            -- a = a + 1 = 13
        ,Load (Addr 2) RegA
        ,Pop RegB                       -- pop return address
        ,Push RegA                      -- push return value
        ,Jump (Ind RegB)                -- jump to return address: 46
        ,Const 3 RegA
        ,Compute Add RegA PC RegA
        ,Store RegA (Addr 1)            -- task Test starts at 25
        ,Jump (Rel 36)                  -- skip Test: 60
        ,Pop RegA
        ,Store RegA (Addr 2)            -- h = 3
        ,Const 4 RegA
        ,Load (Addr 2) RegB
        ,Compute Mul RegA RegB RegA
        ,Store RegA (Addr 2)            -- h = 4*h = 12
        ,Load (Addr 3) RegA
        ,Push RegA
        ,Load (Addr 2) RegA
        ,Push RegA
        ,Load (Addr 1) RegA
        ,Push RegA
        ,Load (Addr 0) RegA
        ,Push RegA                      -- push known addresses
        ,Const 6 RegA
        ,Compute Add RegA PC RegA
        ,Push RegA                      -- push return address 46
        ,Load (Addr 2) RegA
        ,Push RegA                      -- push arg h=12
        ,Load (Addr 0) RegA
        ,Jump (Ind RegA)                -- jump to TestH
        ,Pop RegA                       -- pop return value
        ,Pop RegB
        ,Store RegB (Addr 0)
        ,Pop RegB
        ,Store RegB (Addr 1)
        ,Pop RegB
        ,Store RegB (Addr 2)
        ,Pop RegB
        ,Store RegB (Addr 3)            -- pop known addresses
        ,Store RegA (Addr 3)            -- n = TestH(h) = 13
        ,Load (Addr 3) RegA
        ,Pop RegB                       -- pop return address
        ,Push RegA                      -- push return value
        ,Jump (Ind RegB)                -- jump to return address
        ,Load (Addr 2) RegA             -- AFTER Test
        ,Push RegA
        ,Load (Addr 1) RegA
        ,Push RegA
        ,Load (Addr 0) RegA
        ,Push RegA                      -- push known addresses
        ,Const 6 RegA
        ,Compute Add RegA PC RegA
        ,Push RegA                      -- push return address 73
        ,Const 3 RegA
        ,Push RegA                      -- push arg 3
        ,Load (Addr 1) RegA
        ,Jump (Ind RegA)                -- jump to Test
        ,Pop RegA                       -- pop return value
        ,Pop RegB
        ,Store RegB (Addr 0)
        ,Pop RegB
        ,Store RegB (Addr 1)
        ,Pop RegB
        ,Store RegB (Addr 2)            -- pop known addresses
        ,Store RegA (Addr 2)            -- k = Test(3) = 13
        ,EndProg]

fib:: [Instruction] 
fib = [Const 3 RegA
        ,Compute Add RegA PC RegA
        ,Store RegA (Addr 0)            -- task fib starts at 353
        ,Jump (Rel 78)                  -- skip to 430
        ,Pop RegA                       
        ,Store RegA (Addr 1)            -- n = 3
        ,Const 0 RegA
        ,Store RegA (Addr 2)
        ,Load (Addr 1) RegA
        ,Const 0 RegB
        ,Compute Equal RegA RegB RegA
        ,Load (Addr 1) RegB
        ,Const 1 RegC
        ,Compute Equal RegB RegC RegB
        ,Compute Or RegA RegB RegA      -- n=0 || n=1
        ,Const 1 RegB
        ,Compute Xor RegA RegB RegA     -- if true, then false and the otherway around
        ,Const 5 RegB
        ,Compute Add RegB PC RegB
        ,Branch RegA (Ind RegB)         -- jump if false to 372
        ,Const 1 RegA
        ,Store RegA (Addr 2)
        ,Jump (Rel 55)
        ,Load (Addr 3) RegA             -- OTHERWISE
        ,Push RegA
        ,Load (Addr 2) RegA
        ,Push RegA
        ,Load (Addr 1) RegA
        ,Push RegA
        ,Load (Addr 0) RegA
        ,Push RegA                      -- push known addresses
        ,Const 8 RegA
        ,Compute Add RegA PC RegA
        ,Push RegA                      -- push return address 389
        ,Load (Addr 1) RegA
        ,Const 1 RegB
        ,Compute Sub RegA RegB RegA
        ,Push RegA                      -- push arg n-1
        ,Load (Addr 0) RegA
        ,Jump (Ind RegA)                -- jump to Fib
        ,Pop RegA
        ,Pop RegB
        ,Store RegB (Addr 0)
        ,Pop RegB
        ,Store RegB (Addr 1)
        ,Pop RegB
        ,Store RegB (Addr 2)
        ,Pop RegB
        ,Store RegB (Addr 3)
        ,Load (Addr 3) RegA
        ,Push RegA
        ,Load (Addr 2) RegA
        ,Push RegA
        ,Load (Addr 1) RegA
        ,Push RegA
        ,Load (Addr 0) RegA
        ,Push RegA
        ,Const 8 RegA
        ,Compute Add RegA PC RegA
        ,Push RegA
        ,Load (Addr 1) RegA
        ,Const 2 RegB
        ,Compute Sub RegA RegB RegA
        ,Push RegA
        ,Load (Addr 0) RegA
        ,Jump (Ind RegA)
        ,Pop RegB
        ,Pop RegC
        ,Store RegC (Addr 0)
        ,Pop RegC
        ,Store RegC (Addr 1)
        ,Pop RegC
        ,Store RegC (Addr 2)
        ,Pop RegC
        ,Store RegC (Addr 3)
        ,Compute Add RegA RegB RegA
        ,Store RegA (Addr 2)
        ,Load (Addr 2) RegA
        ,Pop RegB
        ,Push RegA
        ,Jump (Ind RegB)
        ,Load (Addr 1) RegA             -- AFTER Fib
        ,Push RegA
        ,Load (Addr 0) RegA
        ,Push RegA                      -- push known addresses
        ,Const 6 RegA
        ,Compute Add RegA PC RegA
        ,Push RegA                      -- push return address 441
        ,Const 3 RegA
        ,Push RegA                      -- push arg 3
        ,Load (Addr 0) RegA
        ,Jump (Ind RegA)                -- jump to Fib
        ,Pop RegA
        ,Pop RegB
        ,Store RegB (Addr 0)
        ,Pop RegB
        ,Store RegB (Addr 1)
        ,Store RegA (Addr 1)
        ,EndProg]
        
fib2 = [Const 3 RegA
        ,Compute Add RegA PC RegA
        ,Store RegA (Addr 0)
        ,Jump (Rel 86)                  -- skip task Fib: 538
        ,Pop RegA
        ,Store RegA (Addr 1)            -- 1:n = 3, 2:n=2
        ,Const 0 RegA
        ,Store RegA (Addr 2)            -- k = 0
        ,Load (Addr 1) RegA
        ,Const 0 RegB
        ,Compute Equal RegA RegB RegA
        ,Load (Addr 1) RegB
        ,Const 1 RegC
        ,Compute Equal RegB RegC RegB
        ,Compute Or RegA RegB RegA      -- n=0 || n=1
        ,Const 1 RegB
        ,Compute Xor RegA RegB RegA
        ,Const 5 RegB
        ,Compute Add RegB PC RegB
        ,Branch RegA (Ind RegB)         -- jump if false: 472
        ,Const 1 RegA
        ,Store RegA (Addr 2)
        ,Jump (Rel 63)
        ,Load (Addr 3) RegA
        ,Push RegA
        ,Load (Addr 2) RegA
        ,Push RegA
        ,Load (Addr 1) RegA
        ,Push RegA
        ,Load (Addr 0) RegA
        ,Push RegA
        ,Const 8 RegA
        ,Compute Add RegA PC RegA
        ,Push RegA                      -- push return address 489
        ,Load (Addr 1) RegA
        ,Const 1 RegB
        ,Compute Sub RegA RegB RegA
        ,Push RegA                      -- push n-1
        ,Load (Addr 0) RegA
        ,Jump (Ind RegA)                -- jump to task Fib
        ,Pop RegA                       -- pop return value
        ,Pop RegB
        ,Store RegB (Addr 0)
        ,Pop RegB
        ,Store RegB (Addr 1)
        ,Pop RegB
        ,Store RegB (Addr 2)
        ,Pop RegB
        ,Store RegB (Addr 3)
        ,Store RegA (Addr 3)            -- l = Fib(n-1)
        ,Load (Addr 4) RegA
        ,Push RegA
        ,Load (Addr 3) RegA
        ,Push RegA
        ,Load (Addr 2) RegA
        ,Push RegA
        ,Load (Addr 1) RegA
        ,Push RegA
        ,Load (Addr 0) RegA
        ,Push RegA
        ,Const 8 RegA
        ,Compute Add RegA PC RegA
        ,Push RegA                      -- push return address 518
        ,Load (Addr 1) RegA
        ,Const 2 RegB
        ,Compute Sub RegA RegB RegA
        ,Push RegA                      -- push n-2
        ,Load (Addr 0) RegA
        ,Jump (Ind RegA)                -- jump to Fib
        ,Pop RegA                       -- pop return value
        ,Pop RegB
        ,Store RegB (Addr 0)
        ,Pop RegB
        ,Store RegB (Addr 1)
        ,Pop RegB
        ,Store RegB (Addr 2)
        ,Pop RegB
        ,Store RegB (Addr 3)
        ,Pop RegB
        ,Store RegB (Addr 4)
        ,Store RegA (Addr 4)            -- r = Fib(n-2)
        ,Load (Addr 3) RegA
        ,Load (Addr 4) RegB
        ,Compute Add RegA RegB RegA
        ,Store RegA (Addr 2)            -- k = l+r
        ,Load (Addr 2) RegA
        ,Pop RegB                       -- pop return address
        ,Push RegA                      -- push return value
        ,Jump (Ind RegB)                -- jump to return address
        ,Load (Addr 1) RegA                 -- AFTER task Fib
        ,Push RegA
        ,Load (Addr 0) RegA
        ,Push RegA
        ,Const 6 RegA
        ,Compute Add RegA PC RegA
        ,Push RegA                          -- push return address 549
        ,Const 3 RegA
        ,Push RegA                          -- push arg 3
        ,Load (Addr 0) RegA
        ,Jump (Ind RegA)                    -- jump to task Fib
        ,Pop RegA
        ,Pop RegB
        ,Store RegB (Addr 0)
        ,Pop RegB
        ,Store RegB (Addr 1)
        ,Store RegA (Addr 1)
        ,EndProg]
        
fib3 = [Const 3 RegA,Compute Add RegA PC RegA,Store RegA (Addr 0),Jump (Rel 86),Pop RegA,Store RegA (Addr 1),Const 0 RegA,Store RegA (Addr 2),Load (Addr 1) RegA,Const 0 RegB,Compute Equal RegA RegB RegA,Load (Addr 1) RegB,Const 1 RegC,Compute Equal RegB RegC RegB,Compute Or RegA RegB RegA,Const 1 RegB,Compute Xor RegA RegB RegA,Const 5 RegB,Compute Add RegB PC RegB,Branch RegA (Ind RegB),Const 1 RegA,Store RegA (Addr 2),Jump (Rel 63),Load (Addr 3) RegA,Push RegA,Load (Addr 2) RegA,Push RegA,Load (Addr 1) RegA,Push RegA,Load (Addr 0) RegA,Push RegA,Const 8 RegA,Compute Add RegA PC RegA,Push RegA,Load (Addr 1) RegA,Const 1 RegB,Compute Sub RegA RegB RegA,Push RegA,Load (Addr 0) RegA,Jump (Ind RegA),Pop RegA,Pop RegB,Store RegB (Addr 0),Pop RegB,Store RegB (Addr 1),Pop RegB,Store RegB (Addr 2),Pop RegB,Store RegB (Addr 3),Store RegA (Addr 3),Load (Addr 4) RegA,Push RegA,Load (Addr 3) RegA,Push RegA,Load (Addr 2) RegA,Push RegA,Load (Addr 1) RegA,Push RegA,Load (Addr 0) RegA,Push RegA,Const 8 RegA,Compute Add RegA PC RegA,Push RegA,Load (Addr 1) RegA,Const 2 RegB,Compute Sub RegA RegB RegA,Push RegA,Load (Addr 0) RegA,Jump (Ind RegA),Pop RegA,Pop RegB,Store RegB (Addr 0),Pop RegB,Store RegB (Addr 1),Pop RegB,Store RegB (Addr 2),Pop RegB,Store RegB (Addr 3),Pop RegB,Store RegB (Addr 4),Store RegA (Addr 4),Load (Addr 3) RegA,Load (Addr 4) RegB,Compute Add RegA RegB RegA,Store RegA (Addr 2),Load (Addr 2) RegA,Pop RegB,Push RegA,Jump (Ind RegB),Load (Addr 1) RegA,Push RegA,Load (Addr 0) RegA,Push RegA,Const 6 RegA,Compute Add RegA PC RegA,Push RegA,Const 2 RegA,Push RegA,Load (Addr 0) RegA,Jump (Ind RegA),Pop RegA,Pop RegB,Store RegB (Addr 0),Pop RegB,Store RegB (Addr 1),Store RegA (Addr 1),EndProg]








debug :: SystemState -> String
debug SysState{..}  
                    | (localMem (sprs !! 0) !!! 1) == 1 = "Second shared memaddr equals 1.\n"
                    | (localMem (sprs !! 0) !!! 1) == 2 = "Second shared memaddr equals 2.\n"
                    | (localMem (sprs !! 0) !!! 1) == 3 = "Second shared memaddr equals 3.\n"
                    | (localMem (sprs !! 0) !!! 1) == 4 = "Second shared memaddr equals 4.\n"
                    | (localMem (sprs !! 0) !!! 1) == 5 = "Second shared memaddr equals 5.\n"
                    | (localMem (sprs !! 0) !!! 1) == 6 = "Second shared memaddr equals 6.\n"
                    | (localMem (sprs !! 0) !!! 1) == 13 = "Second shared memaddr equals 13.\n"
                    
                    
                    
                    -- | (localMem (sprs !! 0) !!! 2) == 0 = "Second shared memaddr equals 0.\n"
                    
debug _ = "Nope\n"

main = runDebug debug 1 fib3
