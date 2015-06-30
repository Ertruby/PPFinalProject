module Main where


import Prelude
import System.IO
import Parse (parse0, showAST)
import Checker (check, countLines)


compile :: FilePath -> IO()
compile input = do
        putStr "\nCompiling started:\n"
        h <- openFile input ReadMode
        contents <- hGetContents h
        let ast = parse0 contents
        let checked = check ast
        putStr "No code generation yet.. \n"
        putStr "Compiling done!\n"
        putStr ("Number of lines compiled: " ++ show (Checker.countLines checked) ++ "\n")
        
drawTree :: FilePath -> IO()
drawTree input = do
        h <- openFile input ReadMode
        contents <- hGetContents h
        let ast = parse0 contents
        Parse.showAST ast
        