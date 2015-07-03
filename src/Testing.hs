module Testing where

import System.IO
import System.FilePath
import System.Directory
import Parse (parse0, showAST)
import Checker (testCheck, check, countLines)
import Parse
import TreeWalker (writeToFile)
import Data.List
import Data.Char
import DataTypesEtc
import TestOutcomes

-- gwn de ast printen en in de 'testOutCome.hs' poten als variabele
-- om de ast te krijgen gebruik die comment achter de if ipv de if statement
-- zo is t redelijk te doen, gwn 1 file waar alles in zit, dan 1 of 2 met een paar foutjes zoals een punt die mist.
-- VERWIJDER DEZE COMMENT !!

testWrong = testSyntax "Input/DeclAssignTest.txt" testOWrong
synTest1 = testSyntax "Input/DeclAssignTest.txt" testO1
        
testSyntax :: FilePath -> AST -> IO()
testSyntax input expected = do
        h <- openFile input ReadMode
        contents <- hGetContents h
        
        let actual = parse0 contents
        let result = actual == expected
        
        let trtrn = if result -- show actual
                        then "no syntactical errors found\n"
                        else "syntactical errors found\n"
        putStr trtrn
        
testContext input = do
        h <- openFile input ReadMode
        contents <- hGetContents h
        let ast = parse0 contents
        let trtrn = if testCheck ast 
                        then "no contextual errors found"
                        else "contextual errors found"
        print trtrn
        check ast
