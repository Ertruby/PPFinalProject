{-# LANGUAGE FlexibleInstances #-}

module TreeWalker where

import DataTypesEtc
import Debug.Trace
import Data.Int
import Sprockell.System

-- | The 'writeToFile' function generates a string which can be written to a file.
-- | It takes two arguments: the AST tree and the file name
writeToFile:: [AST] -> String -> String 
writeToFile ast s = "{-# LANGUAGE RecordWildCards #-} \nmodule Output." ++ s 
    ++ " where \nimport Sprockell.System\nprog:: [Instruction] \nprog = " 
    ++ show ((walkTree ast []) ++ [EndProg]) ++ "\nmain = run 1 prog >> putChar '\\n'"

-- | The 'walkTree' function generates a list of SPRIL instructions
-- | It takes two arguments: the AST tree and a list of tuples (var/task name,address)
walkTree:: [AST] -> [(String, Address)] -> [Instruction]
walkTree [] _ = []
walkTree (n:ns) addrList  = case n of
    ASTNode Program [f,b]
            -> walkTree [b] addrList  ++ walkTree ns addrList 
    ASTNode ProgBody ls
            -> walkTree ls addrList  ++ walkTree ns addrList 
    ASTNode Task [ASTNode FuncName [ASTLeaf s],ASTNode Args a,ASTNode Type [ASTLeaf t],b]
            -> [Const 3 RegA, Compute Add RegA PC RegA, Store RegA (Addr addrC), Jump (Rel after)] 
                ++ bodyInstr ++ walkTree ns newAddrList
                where
                    bodyInstr = ins ++ walkTree [b] bodyAddrList ++ pushRetValue
                    pushRetValue = if (t /= "TypeNothing") 
                                        then [Pop RegB, Push RegA, Jump (Ind RegB)]
                                        else [Pop RegB, Jump (Ind RegB)]
                    (ins, bodyAddrList) = popArgs a ([],newAddrList) 
                    after = fromIntegral ((length bodyInstr) + 1 :: Int) :: Int32
                    addrC = fromIntegral ((length addrList) :: Int) :: Int32 
                    newAddrList = (s,addrC):addrList 
    ASTNode FuncCall (ASTNode FuncName [ASTLeaf f]:args)
            -> pushAddresses addrC ++ [Const retAddr RegA, Compute Add RegA PC RegA, Push RegA] ++ ins 
                ++ [Load (Addr (head [addr | (i, addr) <- addrList, f == i])) RegA, Jump (Ind RegA)]
                ++ popAddresses addrC RegA ++ walkTree ns addrList
                where
                    ins = pushArgs args addrList
                    addrC = fromIntegral ((length addrList) :: Int) :: Int32 
                    retAddr = fromIntegral ((length ins) + 4 :: Int) :: Int32 
    ASTNode Idf [ASTLeaf s] -- give
            -> [Load (Addr (head [addr | (i, addr) <- addrList, s == i])) RegA]
    ASTNode Body ls
            -> walkTree ls addrList  ++ walkTree ns addrList 
    ASTNode When (e:b:os)
            -> (evalExpr [e] addrList RegA) ++ [Const 1 RegB] ++ [Compute Xor RegA RegB RegA]
                ++ [Const l RegB, Compute Add RegB PC RegB, Branch RegA (Ind RegB)] 
                ++ bi ++ [Jump (Rel skipOs)] ++ obi ++ walkTree ns addrList 
                where 
                    bi = walkTree [b] addrList 
                    obi = walkTree os addrList
                    l = fromIntegral ((length bi)+3 :: Int) :: Int32
                    skipOs = fromIntegral ((length obi)+1 :: Int) :: Int32
    ASTNode While [e,b]
            -> [Compute Add PC Zero RegA, Push RegA] ++ (evalExpr [e] addrList RegA) 
                ++ [Const 1 RegB] ++ [Compute Xor RegA RegB RegA]
                ++ [Const l RegB, Compute Add RegB PC RegB, Branch RegA (Ind RegB)] 
                ++ bi ++ [Pop RegA, Jump (Ind RegA), Pop RegA] ++ walkTree ns addrList 
                where 
                    bi = walkTree [b] addrList 
                    l = fromIntegral ((length bi)+4 :: Int) :: Int32
    ASTNode Decl (t:i:e)
            | checkTypeArray t && length e > 0 -> 
                arrayToIns e newAddrList (getIdf i) arrayLength
                ++ walkTree ns newAddrList
            | length e > 0 -> (evalExpr e addrList RegA) ++ [Store RegA (Addr addrC)] 
                ++ walkTree ns ((getIdf i, addrC):addrList) 
            | otherwise -> [Const 0 RegA] ++ [Store RegA (Addr addrC)] 
                ++ walkTree ns ((getIdf i, addrC):addrList) 
                where 
                    addrC = fromIntegral ((length addrList) :: Int) :: Int32 
                    arrayLength = getLength e 
                    newAddrList = allocArray addrList (getIdf i) (arrayLength-1) 
    ASTNode Assign [i,e@(ASTNode Expr [ASTNode Array _])]
            -> arrayToIns [e] addrList (getIdf i) (getLength [e])
                ++ walkTree ns addrList 
    ASTNode Assign [ASTNode Idf [ASTLeaf i, ex@(ASTNode Expr _)],e]
            -> evalExpr [ex] addrList RegA ++ evalExpr [e] addrList RegC 
                ++ [Const addrC RegB, Compute Add RegA RegB RegA, Store RegC (Deref RegA)]
                ++ walkTree ns addrList 
                where
                    addrC = fromIntegral (test :: Int32) :: Value
                    test = head [addr | (s, addr) <- addrList, s == str]
                    str = i ++ "[" ++ show(0) ++ "]"
    ASTNode Assign [i,e]
            -> (evalExpr [e] addrList RegA) 
                ++ [Store RegA (Addr (head test))]
                ++ walkTree ns addrList 
                where
                    test = [addr | (s, addr) <- addrList, s == getIdf i]
    ASTNode DataTypesEtc.Incr [i]
            -> evalExpr [i] addrList RegA 
                ++ [Const 1 RegB, Compute Add RegA RegB RegA, Store RegA (Addr (head test))]
                ++ walkTree ns addrList 
                where
                    test = [addr | (s, addr) <- addrList, s == getIdf i]
    _ -> error ("walkTree " ++ show n)

    
    
    
-- | The 'evalExpr' function generates a list of instructions from an expression
-- | It takes an AST sub-tree, the list with known addresses and an output register
evalExpr:: [AST] -> [(String, Address)] -> Reg -> [Instruction]
evalExpr [] _ _ = []
evalExpr (n:ns) addrList reg = case n of 
    ASTNode Expr [l,op,r]
            -> evalExpr [l] addrList reg ++ evalExpr [r] addrList (getNextReg regList reg) 
                ++ evalExpr [op] addrList reg
    
    ASTNode Integer [ASTLeaf s] 
            -> [Const (read s :: Value) reg]
    ASTNode Boolean [ASTLeaf s] 
            | s == "true" -> [Const 1 reg]
            | otherwise -> [Const 0 reg]
    ASTNode Character [ASTLeaf s] 
            -> [Const (ord (head s)) reg] 
    ASTNode Idf [ASTLeaf s] 
            -> [Load (Addr (head [addr | (i, addr) <- addrList, s == i])) reg]
    ASTNode Idf [ASTLeaf i, e]
            -> evalExpr [e] addrList reg1 
                ++ [Const addrC reg, Compute Add reg1 reg reg1, Load (Deref reg1) reg]
                where
                    reg1 = getNextReg regList reg
                    addrC = fromIntegral (test :: Int32) :: Value
                    test = head [addr | (s, addr) <- addrList, s == str]
                    str = i ++ "[" ++ show(0) ++ "]"
    ASTNode FuncCall (ASTNode FuncName [ASTLeaf f]:args)
            -> pushAddresses addrC ++ [Const retAddr RegA, Compute Add RegA PC RegA, Push RegA] ++ ins 
                ++ [Load (Addr (head [addr | (i, addr) <- addrList, f == i])) RegA, Jump (Ind RegA)]
                 ++ [Pop reg] ++ popAddresses addrC (getNextReg regList reg) 
                where
                    ins = pushArgs args addrList
                    retAddr = fromIntegral ((length ins) + 4 :: Int) :: Int32 
                    addrC = fromIntegral ((length addrList) :: Int) :: Int32
            
    ASTLeaf op
            -> [Compute (getOp opsStr ops op) reg (getNextReg regList reg) reg]  

    ASTNode _ ls -- skips: Value, Expr with 1 child
            -> evalExpr ls addrList reg



-- | The 'arrayToIns' function generates instructions from an expression of type array
-- | It takes an AST sub-tree, list of known addresses, identifier and the array size
arrayToIns:: [AST] -> [(String, Address)] -> String -> Int -> [Instruction]    
arrayToIns [ASTNode Array v@(l:ls)] addrList i size
    | ls == [] = (evalExpr [l] addrList RegA) ++ [Store RegA (Addr addr)] 
    | otherwise = (evalExpr [l] addrList RegA) ++ [Store RegA (Addr addr)] 
                ++ arrayToIns [ASTNode Array ls] addrList i size
                where 
                    addr = head [addr | (s, addr) <- addrList, s == idf]
                    idf = i ++ "[" ++ show (size - length v) ++ "]"
arrayToIns [ASTNode Expr ls] addrList i size = arrayToIns ls addrList i size
arrayToIns [ASTNode Value ls] _ _ _ = []                
            
            
-- | The 'pushArgs' function generates instructions to push all arguments of a function call
-- | It takes an AST sub-tree and the list of known addresses        
pushArgs:: [AST] -> [(String, Address)] -> [Instruction]
pushArgs [] addrList = []
pushArgs (n:ns) addrList = pushArgs ns addrList ++ evalExpr [n] addrList RegA ++ [Push RegA]
    
-- | The 'popArgs' function generates instructions to pop all arguments from the stack and declares them
-- | It takes an AST sub-tree and a tuple of (instructions,address list) from the previous iteration
popArgs:: [AST] -> ([Instruction],[(String, Address)]) -> ([Instruction],[(String, Address)])
popArgs [] res = res
popArgs ((ASTNode Arg [t,i]):ns) (ins,addrList) 
    = popArgs ns (ins ++ [Pop RegA, Store RegA (Addr addrC)], ((getIdf i, addrC):addrList))
    where 
        addrC = fromIntegral ((length addrList) :: Int) :: Int32

-- | The 'pushAddresses' function generates instructions to push the values of the known addresses
-- | It takes the address to push
pushAddresses:: Address -> [Instruction]
pushAddresses addrC 
    | addrC == 0 = [Load (Addr addrC) RegA, Push RegA]
    | otherwise = [Load (Addr addrC) RegA, Push RegA] ++ pushAddresses (addrC-1) 
    
-- | The 'popAddresses' function generates instructions to pop all values and store them back to
--      their assigned address
-- | Takes the address to store the popped value at and the register to use
popAddresses:: Address -> Reg -> [Instruction]
popAddresses addrC reg
    | addrC == 0 = [Pop reg, Store reg (Addr addrC)]
    | otherwise = popAddresses (addrC-1) reg ++ [Pop reg, Store reg (Addr addrC)]
    
-- | The 'allocArray' function allocates memory addresses for an array
-- | It takes the address list, the identifier of the array and the array length
allocArray:: [(String, Address)] -> String -> Int -> [(String, Address)]
allocArray addrList idf i
    | i == 0 = (idf ++ "[" ++ show(i) ++ "]", addrC):addrList
    | otherwise = (idf ++ "[" ++ show(i) ++ "]", addrC):(allocArray addrList idf (i-1))
    where 
        addrC = fromIntegral ((length addrList)+i :: Int) :: Int32

-- | The 'checkTypeArray' function checks whether the type is type array
-- | It takes an AST sub-tree
checkTypeArray:: AST -> Bool
checkTypeArray (ASTNode Type [ASTNode TypeArray _]) = True
checkTypeArray _ = False

-- | The 'getLength' function returns the specified length of the array
-- | It takes an AST sub-tree
getLength:: [AST] -> Int
getLength [ASTNode Expr [ASTNode Array ls]] = length ls
getLength [ASTNode Expr [ASTNode Value [ASTNode Integer [ASTLeaf n]]]] = read n :: Int
    
regList = [RegA,RegB,RegC,RegD,RegE]

-- | The 'getNextReg' function takes the next register from the given list
-- | It takes a list of registers and the current register
getNextReg:: [Reg] -> Reg -> Reg
getNextReg (r:rs) t | r == t && length rs >= 1 = head rs
                    | r /= t = getNextReg rs t
                    | otherwise = regList!!0
   
   
ops = [Add,Sub,Mul,Div,Mod,Equal,NEq,Gt,GtE,Lt,LtE,And,Or,Xor,LShift,RShift]
opsStr = ["plus", "minus", "times", "DividedBy", "modulo", "equals", "notEquals", "GreaterThan",
        "GreaterThanEq", "SmallerThan", "SmallerThanEq", "and", "or", "Xor", "LShift", "RShift"]
        
-- | The 'getOp' function returns the SPRIL operator associated with the given string
-- | It takes a list of operators in the form of strings, a list of Operator and an operator string
getOp :: [String] -> [Operator] -> String -> Operator
getOp [] [] s = error ("operator not found" ++ show(s))
getOp (x:xs) (y:ys) s   | s == x = y
                        | otherwise = getOp xs ys s
                                
-- | The 'getIdf' function returns the identifier as a string
-- | It takes an AST sub-tree
getIdf:: AST -> String
getIdf (ASTNode Idf [ASTLeaf s]) = s
getIdf n = error ("getIdf" ++ show(n))
