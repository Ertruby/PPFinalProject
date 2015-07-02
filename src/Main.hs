module Main where


import Prelude
import System.IO
import Parse (parse0, showAST)
import Checker (check, countLines)
import TreeWalker
import Data.List
import Data.Char


compile :: FilePath -> IO()
compile input = do
        putStr "\nCompiling started:\n"
        h <- openFile input ReadMode
        contents <- hGetContents h
        let ast = parse0 contents
        check ast
        let fileNameP = if isSuffixOf ".txt" input
                        then take ((length input)-5) input
                        else input
        let fileName = [toUpper (head fileNameP)] ++ tail fileNameP
        outh <- openFile ("Output/" ++ fileName ++ ".hs") WriteMode
        hPutStrLn  outh (writeToFile [ast] fileName)
        hClose outh
        
        -- putStr ("Instructions: \n" ++ show(instr) ++ "\n")
        putStr "Compiling done!\n"
        putStr ("Number of lines compiled: " ++ show (Checker.countLines ast) ++ "\n")
        
drawTree :: FilePath -> IO()
drawTree input = do
        h <- openFile input ReadMode
        contents <- hGetContents h
        let ast = parse0 contents
        Parse.showAST ast
        