{-# LANGUAGE FlexibleInstances #-}

module TreeWalker where

import Parse
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

test99 = walkTree [toAST test0] [] 1

walkTree:: [AST] -> [(String, Address)] -> Address -> [Instruction]
walkTree [] _ _ = [EndProg]
walkTree (n:ns) addrList addrC = case n of
    ASTNode Decl [t,i]      
            -> [Const 0 RegA, Store RegA (Addr addrC)] ++ walkTree ns ((getIdf i, addrC):addrList) (addrC+1)
    ASTNode Decl [t,i,e]
            -> (evalExpr [e] addrList RegA) ++ [Store RegA (Addr addrC)] ++ walkTree ns ((getIdf i, addrC):addrList) (addrC+1)
    _ -> error ("walkTree " ++ show n)
    -- ASTNode Decl [t:i:es]      
            -- -> (walkTree [es] addrList addrC) ++ [Store RegA addrC] ++ walkTree ns ((getIdf i, addrC):addrList) (addrC+1)
    -- ASTNode Expr ls
            -- -> walkTree ls addrList addrC
    -- ASTNode Value ls 
            -- -> walkTree ls addrList addrC
    -- ASTNode _ ls
            -- -> walkTree ls addrList addrC
    -- ASTNode Integer [ASTLeaf s] 
            -- -> [Const (read s :: Integer) RegA]

regList = [RegA,RegB,RegC,RegD,RegE] 
getNextReg:: [Reg] -> Reg -> Reg
getNextReg (r:rs) t | r == t && length rs >= 1 = head rs
                    | r /= t = getNextReg rs t
                    | otherwise = regList!!0

evalExpr:: [AST] -> [(String, Address)] -> Reg -> [Instruction]
evalExpr (n:ns) addrList reg = case n of 
    ASTNode Expr [l,op,r]
            -> evalExpr [l] addrList reg ++ evalExpr [r] addrList (getNextReg regList reg) ++ evalExpr [op] addrList reg
    
    ASTNode Integer [ASTLeaf s] 
            -> [Const (read s :: Value) reg]
    ASTNode Boolean [ASTLeaf s] 
            | s == "true" -> [Const 1 reg]
            | otherwise -> [Const 0 reg]
    ASTNode Character [ASTLeaf s] 
            -> [Const (ord (head s)) reg]
    ASTNode Idf [ASTLeaf s] 
            -> [Load (Addr (head [addr | (s, addr) <- addrList])) reg]
            
    ASTLeaf op
            -> [Compute (getOp opsStr ops op) RegA RegB reg]
            

    ASTNode _ ls -- skips: Value, Expr with 1 child
            -> evalExpr ls addrList reg         
    _ -> error ("evalExpr " ++ show n)
    
     

-- getType:: AST -> Alphabet
-- getType (ASTNode Type [ASTLeaf s])  | s == "TypeBool" = TypeBool
                                    -- | s == "TypeBool" = TypeInt
                                    -- | s == "TypeBool" = TypeChar
                                    -- | s == "TypeBool" = TypeNothing
                                    -- | otherwise = error "fucktard"
                                    
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
