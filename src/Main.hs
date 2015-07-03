module Main where

import System.IO
import System.FilePath
import Parse (parse0, showAST)
import Checker (check, countLines)
import TreeWalker (writeToFile)
import Data.List
import Data.Char

-- compile takes a string that represents a file path so it can read that file.
-- gives the string to parse0 to get an AST, then gets the AST checked by check. if check finds errors it will stop here.
-- if the code is OK, it will generate code and an output file.
compile :: FilePath -> IO()
compile input = do
        putStr "\nCompiling started:\n"
        h <- openFile input ReadMode
        contents <- hGetContents h
        let ast = parse0 contents
        check ast
        let fileNameP = if isSuffixOf ".txt" input
                        then take ((length input)-4) (takeFileName input)
                        else takeFileName input
        let fileName = [toUpper (head fileNameP)] ++ tail fileNameP
        outh <- openFile ("Output/" ++ fileName ++ ".hs") WriteMode
        hPutStrLn  outh (writeToFile [ast] fileName)
        hClose outh
        putStr "Compiling done!\n"
        putStr ("Number of lines compiled: " ++ show (Checker.countLines ast) ++ "\n")
 
-- also takes a string that represents a file path. will parse the file and then print the AST on the standard web page 2. 
drawTree :: FilePath -> IO()
drawTree input = do
        h <- openFile input ReadMode
        contents <- hGetContents h
        let ast = parse0 contents
        Parse.showAST ast
        