{-# LANGUAGE RecordWildCards #-}
module Testing where

import System.IO
import System.FilePath
import System.Directory
import Parse (parse0, showAST)
import Checker (testCheck, check, countLines)
import Parse
import TreeWalker (writeToFile, walkTree)
import Data.List
import Data.Char
import DataTypesEtc
import TestOutcomes
import Sprockell.System
import Debug.Trace

        
testSyntax :: FilePath -> AST -> IO()
testSyntax input expected = do
        h <- openFile input ReadMode
        contents <- hGetContents h
        
        let actual = parse0 contents
        let result = actual == expected
        
        let trtrn = if result
                        then "No syntactical errors found"
                        else "No syntactical errors found but the AST's are not the same"
        print trtrn
        

testContext :: FilePath -> IO()
testContext input = do
        h <- openFile input ReadMode
        contents <- hGetContents h
        let ast = parse0 contents
        let trtrn = if testCheck ast 
                        then "no contextual errors found"
                        else "contextual errors found"
        check ast
        print trtrn

drawTree :: FilePath -> IO()
drawTree input = do
        h <- openFile input ReadMode
        contents <- hGetContents h
        let ast = parse0 contents
        Parse.showAST ast


        
        
        
        
        
        
type TestSuite = (String, Int, [Instruction], (SystemState -> String))

getRegs sysState spid regs = getRegs' (regbank ((sprs sysState) !! spid)) regs
    where getRegs' rs = map (rs !)

-- Running test logic --
runSuite :: TestSuite -> IO ()
runSuite (name, nSprockells, prog, test) = do
    putStr name >> putStr " (n=" >> putStr (show nSprockells) >> putStr "): "
    run nSprockells prog >>= return . test >>= putStr
    putChar '\n'
    return ()
    
testSemantic input = do
    h <- openFile input ReadMode
    contents <- hGetContents h
    let ast = parse0 contents
    check ast
    let suite = testSemanticSuite [ast]
    runSuite suite
    return ()
        
testSemanticSuite ast = ("Semantics Test", 1, walkTree ast [] ++ [EndProg], testSemanticTest)
testSemanticTest sysState@SysState{ .. }
        -- change the integer after !!! to the address you want to look at and the 
        -- integer after == to the expected output
         | (localMem (sprs !! 0) !!! 1) == 377 = "Pass"
         | otherwise             = "Fail"
        
        
-- ============================================================================================
-- Syntax tests:

assignSyntax = testSyntax "Input/Syntax/assignTest.txt" assignAST
assignFailSyntax = testSyntax "Input/Syntax/assignFailTest.txt" assignFailAST
commentSyntax = testSyntax "Input/Syntax/commentTest.txt" commentAST
commentFailSyntax = testSyntax "Input/Syntax/commentFailTest.txt" commentFailAST
declSyntax = testSyntax "Input/Syntax/DeclTest.txt" declAST
declFailSyntax = testSyntax "Input/Syntax/DeclFailTest.txt" declFailAST
exprSyntax = testSyntax "Input/Syntax/exprTest.txt" exprAST
exprFailSyntax = testSyntax "Input/Syntax/exprFailTest.txt" exprFailAST
funcCallSyntax = testSyntax "Input/Syntax/funcCallTest.txt" funcCallAST
funcCallFailSyntax = testSyntax "Input/Syntax/funcCallFailTest.txt" funcCallFailAST
incrSyntax = testSyntax "Input/Syntax/incrTest.txt" incrAST
incrFailSyntax = testSyntax "Input/Syntax/incrFailTest.txt" incrFailAST
programSyntax = testSyntax "Input/Syntax/programTest.txt" programAST
programFailSyntax = testSyntax "Input/Syntax/programFailTest.txt" programFailAST
taskSyntax = testSyntax "Input/Syntax/taskTest.txt" taskAST
taskFailSyntax = testSyntax "Input/Syntax/taskFailTest.txt" taskFailAST
whenSyntax = testSyntax "Input/Syntax/whenTest.txt" whenAST
whenFailSyntax = testSyntax "Input/Syntax/whenFailTest.txt" whenFailAST
whileSyntax = testSyntax "Input/Syntax/whileTest.txt" whileAST
whileFailSyntax = testSyntax "Input/Syntax/whileFailTest.txt" whileFailAST

-- ============================================================================================
-- Context tests:

idfContext = testContext "Input/Context/IdfContext.txt" 
idfContextFail = testContext "Input/Context/IdfContextFail.txt" 
typeContext = testContext "Input/Context/TypeContext.txt" 
typeContextFail = testContext "Input/Context/TypeContextFail.txt" 

-- ============================================================================================
-- Semantic tests:
fib = testSemantic "Input/Semantic/fib.txt"
infiniteLoop = testSemantic "Input/Semantic/infiniteLoop.txt"
divisionByZero = testSemantic "Input/Semantic/divisionByZero.txt"