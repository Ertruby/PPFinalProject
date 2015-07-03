module Testing where

import System.IO
import System.FilePath
import System.Directory
import Parse (parse0, showAST)
import Checker (testCheck, countLines)
import TreeWalker (writeToFile)
import Data.List
import Data.Char

        
testSyntax :: FilePath -> IO [Char]
testSyntax input = do
        openFile input ReadMode
        h <- openFile input ReadMode
        contents <- hGetContents h
        let ast = parse0 contents
        let trtrn = if testCheck ast
                        then "No syntax errors found"
                        else "Syntax errors found"
        return trtrn