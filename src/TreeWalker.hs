{-# LANGUAGE FlexibleInstances #-}

module TreeWalker where

import Parse
import Debug.Trace

import Data.Int
import Sprockell.System

-- =========================================================================
-- data AST = ASTNode Alphabet [AST] | ASTLeaf String deriving (Show, Eq)
-- =========================================================================
-- The Alphabet:
-- Program, ProgBody, Decl, Assign, When, While, Task, Arg, FuncName, Body, 
-- ProgLine, Line, Expr, ExprH, Op, GreaterThan, GreaterThanEq, SmallerThan, 
-- SmallerThanEq, Type, Idf, Value, Array, ArrayVal, TypeArray, Boolean, 
-- TypeBool, Integer, TypeInt, Character, TypeChar, TrueK, FalseK, Incr, 
-- VIA, DividedBy, FuncCall, Error, TypeNothing

test99 = walkTree [toAST test0] [] []

walkTree:: [AST] -> [(String, Address)] -> [(String, ([Address],Target))] -> [Instruction]
walkTree [] _ _ = []
walkTree (n:ns) addrList funcList = case n of
    ASTNode Program [f,b]
            -> walkTree [b] addrList funcList ++ walkTree ns addrList funcList
    ASTNode ProgBody ls
            -> walkTree ls addrList funcList ++ walkTree ns addrList funcList
    ASTNode Task ls
            | t /= TypeNothing -> ins ++ [Const 1 RegA, Compute Add RegA PC RegA]
                    ++ walkTree [b] newAddrList newFuncList ++ [Pop RegB, Push RegA, Jump (Ind RegB)] 
                    ++ walkTree ns newAddrList newFuncList
            | otherwise -> ins ++ [Const 1 RegA, Compute Add RegA PC RegA]
                    ++ walkTree [b] newAddrList newFuncList ++ [Pop RegB, Jump (Ind RegB)] 
                    ++ walkTree ns newAddrList newFuncList
                where
                    newFuncList = ((s,([addr | (s, addr) <- argAddrList],Ind RegA)):funcList)
                    newAddrList = (addrList ++ argAddrList)
                    (ins, argAddrList) = handleArgs a (length addrList) ([],[])
    ASTNode Arg [t,i]
            -> [Const 0 RegA] ++ [Store RegA (Addr addrC)] 
                            ++ walkTree ns ((getIdf i, addrC):addrList) funcList
                where 
                    addrC = fromIntegral ((length addrList) :: Int) :: Int32
    ASTNode Idf [ASTLeaf s] -- Tasks give
            -> [Load (Addr (head [addr | (i, addr) <- addrList, s == i])) RegA]
    ASTNode Body ls
            -> walkTree ls addrList funcList ++ walkTree ns addrList funcList
    ASTNode When (e:b:os)
            -> (evalExpr [e] addrList RegA) ++ [Const 1 RegB] ++ [Compute Xor RegA RegB RegA]
                ++ [Const l RegB, Compute Add RegB PC RegB, Branch RegA (Ind RegB)] 
                ++ bi ++ walkTree os addrList funcList ++ walkTree ns addrList funcList
                where 
                    bi = walkTree [b] addrList funcList
                    l = fromIntegral ((length bi)+2 :: Int) :: Int32
    ASTNode While [e,b]
            -> [Compute Add PC Zero RegA, Push RegA] ++ (evalExpr [e] addrList RegA) 
                ++ [Const 1 RegB] ++ [Compute Xor RegA RegB RegA]
                ++ [Const l RegB, Compute Add RegB PC RegB, Branch RegA (Ind RegB)] 
                ++ bi ++ [Pop RegA, Jump (Ind RegA), Pop RegA] ++ walkTree ns addrList funcList
                where 
                    bi = walkTree [b] addrList funcList
                    l = fromIntegral ((length bi)+4 :: Int) :: Int32
    ASTNode Decl (t:i:e)
            | length e > 0 -> (evalExpr e addrList RegA) ++ [Store RegA (Addr addrC)] 
                            ++ walkTree ns ((getIdf i, addrC):addrList) funcList
            | otherwise -> [Const 0 RegA] ++ [Store RegA (Addr addrC)] 
                            ++ walkTree ns ((getIdf i, addrC):addrList) funcList
                where 
                    addrC = fromIntegral ((length addrList) :: Int) :: Int32      
    ASTNode Assign [i,e]
            -> (evalExpr [e] addrList RegA) 
                ++ [Store RegA (Addr (head test))]
                ++ walkTree ns addrList funcList
                where
                    test = [addr | (s, addr) <- addrList, s == getIdf i]
    _ -> error ("walkTree " ++ show n)

evalExpr:: [AST] -> [(String, Address)] -> Reg -> [Instruction]
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
            
    ASTLeaf op
            -> [Compute (getOp opsStr ops op) reg (getNextReg regList reg) reg]  

    ASTNode _ ls -- skips: Value, Expr with 1 child
            -> evalExpr ls addrList reg         
    _ -> error ("evalExpr " ++ show n)

handleTask:: [AST] -> [AST, [AST], AST, AST] -> [AST, [AST], AST, AST]
handleTask (n:ns) [f, args, t, b] = case n of
    ASTNode FuncName
    
handleArgs:: [AST] -> Int -> ([Instruction],[(String, Address)]) -> ([Instruction],[(String, Address)])
handleArgs [] _ res = res
handleArgs (n:ns) l (i,addrList) = case n of
    ASTNode Arg [t,i]
            -> handleArgs ns (l+1) (i ++ [Const 0 RegA] ++ [Store RegA (Addr addrC)], ((getIdf i, addrC):addrList))
                where 
                    addrC = fromIntegral (l :: Int) :: Int32
     

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
