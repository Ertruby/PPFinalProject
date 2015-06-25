import Sprockell.System



prog :: [Instruction]
prog = [
           Const (ord 'A') RegA
         , Const (ord 'a' - ord 'A') RegE

         , Compute Add RegA SPID RegB -- sprockell id as ascii character (uppercase)
         , Compute Add RegB RegE RegC -- (lowercase)

         , Const loopCount RegD
         , Const 1 RegE

         , Write RegB stdio -- write uppercase letter
         , Write RegC stdio -- write lowercase letter
         , Compute Sub RegD RegE RegD
         , Branch RegD (Rel (-3))

         --, Read (Addr 0x0)  -- dummy read to ensure that
         --, Receive RegA     -- all write request are done
         , EndProg
       ]


main = run 1 prog >> putChar '\n'
