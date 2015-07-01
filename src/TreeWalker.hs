{-# LANGUAGE FlexibleInstances #-}

module TreeWalker where

import Parse
import Debug.Trace

import Data.Int
import Sprockell.System

-- test99 = walkTree [toAST test1] []

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
                ++ popAddresses addrC ++ walkTree ns addrList
                where
                    ins = pushArgs args addrList
                    addrC = fromIntegral ((length addrList) :: Int) :: Int32 
                    retAddr = fromIntegral ((length ins) + 4 :: Int) :: Int32 
    ASTNode Idf [ASTLeaf s] -- Tasks give
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
            | checkTypeArray t && length e > 0 -> --traceShow ("success: \n" ++ show(e)) -- ++ "\n" ++ show(newAddrList) ++ "\n") 
                arrayToIns e newAddrList (getIdf i)
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
            -> arrayToIns [e] addrList (getIdf i)
                ++ walkTree ns addrList 
    ASTNode Assign [i,e]
            -> (evalExpr [e] addrList RegA) 
                ++ [Store RegA (Addr (head test))]
                ++ walkTree ns addrList 
                where
                    test = [addr | (s, addr) <- addrList, s == getIdf i]
    ASTNode Parse.Incr [i]
            -> evalExpr [i] addrList RegA 
                ++ [Const 1 RegB, Compute Add RegA RegB RegA, Store RegA (Addr (head test))]
                ++ walkTree ns addrList 
                where
                    test = [addr | (s, addr) <- addrList, s == getIdf i]
    _ -> error ("walkTree " ++ show n)

    
    
    
    
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
    ASTNode FuncCall (ASTNode FuncName [ASTLeaf f]:args)
            -> pushAddresses addrC ++ [Const retAddr RegA, Compute Add RegA PC RegA, Push RegA] ++ ins 
                ++ [Load (Addr (head [addr | (i, addr) <- addrList, f == i])) RegA, Jump (Ind RegA)]
                ++ popAddresses addrC ++ [Pop reg]
                where
                    ins = pushArgs args addrList
                    retAddr = fromIntegral ((length ins) + 4 :: Int) :: Int32 
                    addrC = fromIntegral ((length addrList) :: Int) :: Int32
            
    ASTLeaf op
            -> [Compute (getOp opsStr ops op) reg (getNextReg regList reg) reg]  

    ASTNode _ ls -- skips: Value, Expr with 1 child
            -> evalExpr ls addrList reg

--ASTNode Expr [ASTNode Array [ASTNode Value [ASTNode Integer [ASTLeaf "1"]],ASTNode Value [ASTNode Integer [ASTLeaf "2"]],ASTNode Value [ASTNode Integer [ASTLeaf "3"]]]]    
--[("o[0]",0),("o[1]",1),("o[2]",2),("c",3)]
test12 = arrayToIns [ASTNode Expr [ASTNode Array [ASTNode Value [ASTNode Integer [ASTLeaf "1"]],ASTNode Value [ASTNode Integer [ASTLeaf "2"]],ASTNode Value [ASTNode Integer [ASTLeaf "3"]]]]] [("o[0]",0),("o[1]",1),("o[2]",2),("c",3)] "o"
            
arrayToIns:: [AST] -> [(String, Address)] -> String -> [Instruction]    
arrayToIns [ASTNode Array (l:ls)] addrList i 
    | ls == [] = (evalExpr [l] addrList RegA) ++ [Store RegA (Addr addr)] 
    | otherwise = (evalExpr [l] addrList RegA) ++ [Store RegA (Addr addr)] 
                ++ arrayToIns [ASTNode Array ls] addrList i
                where 
                    addr = head [addr | (s, addr) <- addrList, s == idf]
                    idf = i ++ "[" ++ show (length ls) ++ "]"
                    -- addrC = fromIntegral ((length addrList) :: Int) :: Int32   
arrayToIns [ASTNode Expr ls] addrList i = arrayToIns ls addrList i
arrayToIns [ASTNode Value ls] _ _ = []                
            
            
-- input = [Expr]            
pushArgs:: [AST] -> [(String, Address)] -> [Instruction]
pushArgs [] addrList = []
pushArgs (n:ns) addrList = pushArgs ns addrList ++ evalExpr [n] addrList RegA ++ [Push RegA]
    
popArgs:: [AST] -> ([Instruction],[(String, Address)]) -> ([Instruction],[(String, Address)])
popArgs [] res = res
popArgs ((ASTNode Arg [t,i]):ns) (ins,addrList) 
    = popArgs ns (ins ++ [Pop RegA, Store RegA (Addr addrC)], ((getIdf i, addrC):addrList))
    where 
        addrC = fromIntegral ((length addrList) :: Int) :: Int32
        
-- pushAddresses [("o[0]",0),("o[1]",1),("o[2]",2),("c",3)]
pushAddresses:: Address -> [Instruction]
pushAddresses addrC 
    | addrC == 0 = [Load (Addr addrC) RegA, Push RegA]
    | otherwise = [Load (Addr addrC) RegA, Push RegA] ++ pushAddresses (addrC-1) 
    
popAddresses:: Address -> [Instruction]
popAddresses addrC 
    | addrC == 0 = [Pop RegA, Store RegA (Addr addrC)]
    | otherwise = [Pop RegA, Store RegA (Addr addrC)] ++ popAddresses (addrC-1)  
    
allocArray:: [(String, Address)] -> String -> Int -> [(String, Address)]
allocArray addrList idf i
    | i == 0 = (idf ++ "[" ++ show(i) ++ "]", addrC):addrList
    | otherwise = (idf ++ "[" ++ show(i) ++ "]", addrC):(allocArray addrList idf (i-1))
    where 
        addrC = fromIntegral ((length addrList)+i :: Int) :: Int32
        
checkTypeArray:: AST -> Bool
checkTypeArray (ASTNode Type [ASTNode TypeArray _]) = True
checkTypeArray _ = False

getLength:: [AST] -> Int
getLength [ASTNode Expr [ASTNode Array ls]] = length ls
getLength [ASTNode Expr [ASTNode Value [ASTNode Integer [ASTLeaf n]]]] = read n :: Int
     
-- setArgs:: [AST] -> [(String, Address)] -> [Address] -> [Instruction]
-- setArgs [] _ [] = []
-- setArgs (e:es) addrList (a:addrs) = evalExpr [e] addrList RegA ++ [Store RegA (Addr a)] ++ setArgs es addrList addrs 
-- getType:: AST -> Alphabet
-- getType (ASTNode Type [ASTLeaf s])  | s == "TypeBool" = TypeBool
                                    -- | s == "TypeBool" = TypeInt
                                    -- | s == "TypeBool" = TypeChar
                                    -- | s == "TypeBool" = TypeNothing
                                    -- | otherwise = error "fucktard"
                                    
regList = [RegA,RegB,RegC,RegD,RegE] 
getNextReg:: [Reg] -> Reg -> Reg
getNextReg (r:rs) t | r == t && length rs >= 1 = head rs
                    | r /= t = getNextReg rs t
                    | otherwise = regList!!0
                                    
ops = [Add,Sub,Mul,Div,Mod,Equal,NEq,Gt,GtE,Lt,LtE,And,Or,Xor,LShift,RShift]
opsStr = ["plus", "minus", "times", "DividedBy", "modulo", "equals", "notEquals", "GreaterThan",
        "GreaterThanEq", "SmallerThan", "SmallerThanEq", "and", "or", "Xor", "LShift", "RShift"]
getOp :: [String] -> [Operator] -> String -> Operator
getOp [] [] s = error "operator not found"
getOp (x:xs) (y:ys) s   | s == x = y
                        | otherwise = getOp xs ys s
                                    
getIdf:: AST -> String
getIdf (ASTNode Idf [ASTLeaf s]) = s
getIdf _ = error "getIdf"
