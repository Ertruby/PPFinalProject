module Testing where

import System.IO
import System.FilePath
import System.Directory
import Parse (parse0, showAST)
import Checker (testCheck, check, countLines)
import TreeWalker (writeToFile)
import Data.List
import Data.Char

        
-- testSyntax :: FilePath -> IO [Char]
testSyntax input = do
        h <- openFile input ReadMode
        contents <- hGetContents h
        let ast = parse0 contents
        let trtrn = if testCheck ast 
                        then "no syntactical errors found"
                        else "syntactical errors found"
        print trtrn
        
testContext input = do
        h <- openFile input ReadMode
        contents <- hGetContents h
        let ast = parse0 contents
        let trtrn = if testCheck ast 
                        then "no syntactical errors found"
                        else "syntactical errors found"
        print trtrn
        check ast
