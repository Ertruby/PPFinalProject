module Checker where 

import Debug.Trace
import Data.List
import FPPrac.Trees
import Prelude
import Data.Char
import qualified Data.Maybe
import qualified Data.Text as TXT

import Parse

-- =========================================================
-- type checking.. also includes scope checking
-- =========================================================

-- main function
check :: AST -> IO()-- supposed to take a Program node, works with more node types, but not all of them
check t =            printErrors t (typeCheckScope [t] [])
            
-- printErrors :: AST -> [(AST,String)] -> IO() -- first argument should be the root of the AST of the program
printErrors root errList
    | errList == [] = putStr "Type and scope checking completed\n"
    | otherwise = do
                    putStr "\n===========================================================\n"
                    let l = map (\(t,e) -> ("- Sentence " ++ show (getLineNr root t) ++ ": \n" ++ e ++ "\n")) errList
                    putStr (concat l)
                    putStr "===========================================================\n"
                    error "Errors found, printed them above this line."

-- general type checker per scope
typeCheckScope :: [AST] -> [(String, String)] -> [(AST, String)] -- second argument (varList) should be empty when called by another function
typeCheckScope nodes varList = case nodes of
        []                                                                              -> []
        (t@(ASTNode Decl [_, _]):ts)
            | fst (makeTupleDecl varList t) == "ERR"                                    -> (t,snd (makeTupleDecl varList t)) : typeCheckScope ts varList
            | otherwise                                                                 -> typeCheckScope ts (makeTupleDecl varList t : varList)
        (t@(ASTNode Decl [tp@(ASTNode Type [ASTNode TypeArray kids0]), i, e@(ASTNode Expr [ASTNode Value kids])]):ts)
            | getAndCheckExpr varList (ASTNode Expr [ASTNode Value kids]) /= "TypeInt"  -> (t,"length of array should be an integer") : typeCheckScope ts varList
            | fst tup == "ERR"                                                          -> (t,snd tup) : typeCheckScope ts varList
            | otherwise                                                                 -> typeCheckScope ts (tup : varList) 
            where
                tup = makeTupleDecl varList (ASTNode Decl [tp,i])
        (t@(ASTNode Decl [tp, i, e]):ts)           
            | fst tup == "ERR"                                                             -> (t,snd tup) : typeCheckScope ts varList
            | otherwise                                                                 -> typeCheckScope [ASTNode Assign [i,e]] newVarList ++ typeCheckScope ts newVarList 
            where 
                tup = makeTupleDecl varList (ASTNode Decl [tp,i])
                newVarList = tup : varList
        (t@(ASTNode Assign [t2@(ASTNode Idf [ASTLeaf var, i]), expr]):ts) 
            | iNotOke                                                                   -> (t,"index of array should be an integer at line ") : typeCheckScope ts varList
            | notArray                                                                  -> (t,show var ++ "is not an array") : typeCheckScope ts varList
            | elemType == actual                                                        -> typeCheckScope ts varList
            | otherwise                                                                 -> (t,show var ++ " is array of " ++ elemType ++ " not of " ++ actual ++ "." ): typeCheckScope ts varList
            where 
                iNotOke     = not (getAndCheckExpr varList i == "TypeInt")
                expected    = getType varList t2
                expectedErr = isPrefixOf "ERR" expected
                notArray    = not (isPrefixOf "TypeArray" expected)
                elemType    = Data.Maybe.fromJust (stripPrefix "TypeArray" expected)
                actual      = getAndCheckExpr varList expr
        (t@(ASTNode Assign [t2@(ASTNode Idf [ASTLeaf var]), expr]):ts)
            | isPrefixOf "ERR" expected                                                 -> (t,drop 3 expected) : typeCheckScope ts varList
            | expected == actual || actual == "TypeEmpty"                               -> typeCheckScope ts varList
            | otherwise                                                                 -> (t,show var ++ " is of " ++ expected ++ " not " ++ actual ++ "." ) : typeCheckScope ts varList
            where 
                expected    = getType varList t2
                actual      = getAndCheckExpr varList expr
        (t@(ASTNode Program kids):ts)                                                   -> typeCheckScope kids (("#", "#"):varList)
        (t@(ASTNode Args xs):ts) 
            | tups2 /= []                                                               -> map (\(a,b) -> (t,b)) tups2 ++ typeCheckScope ts varList
            | otherwise                                                                 -> typeCheckScope ts ((map (makeTupleDecl varList) xs) ++ varList)
            where
                tups = (map (makeTupleDecl varList) xs)
                tups2 = filter (\(a,b) -> a == "ERR") tups
        (t@(ASTNode ProgBody kids):ts) 
            | errs /= []                                                                -> map (\(a,b) -> (t,b)) errs ++ typeCheckScope ts varList
            | otherwise                                                                 -> typeCheckScope kids ((getGlobals kids)++varList) ++ typeCheckScope ts varList
            where
                errs = filter (\(a,b) -> a == "ERR") (getGlobals kids)
        (t@(ASTNode Task kids):ts)                                                      -> typeCheckScope kids (("#", "#"):varList) ++ typeCheckScope ts varList
        (t@(ASTNode Body kids):ts)                                                      -> typeCheckScope kids varList ++ typeCheckScope ts varList
        (t@(ASTNode While [condition, body]):ts) 
            | (getAndCheckExpr varList condition) /= "TypeBool"                         -> (t,"While statement should contain a boolean expression") : typeCheckScope ts varList
            | otherwise                                                                 -> typeCheckScope [body] (("#", "#"):varList) ++ typeCheckScope ts varList
        (t@(ASTNode When kids):ts) 
            | (getAndCheckExpr varList (head kids)) /= "TypeBool"                       -> (t,"When statement should contain a boolean expression") : typeCheckScope ts varList
            | otherwise                                                                 -> typeCheckScope (tail kids) (("#", "#"):varList) ++ typeCheckScope ts varList
        (t@(ASTNode Incr [t2@(ASTNode Idf [ASTLeaf var])]):ts)
            | isPrefixOf "ERR" (getType varList t2)                                     -> (t,drop 3 (getType varList t2)) : typeCheckScope ts varList
            | getType varList t2 /= "TypeInt"                                           -> (t,"increment takes an integer variable") : typeCheckScope ts varList
            | otherwise                                                                 -> typeCheckScope ts varList
        (t:ts)                                                                          -> typeCheckScope ts varList

-- get globals variables and functions for initializing the varList
getGlobals :: [AST] -> [(String,String)]
getGlobals [] = []
getGlobals (t@(ASTNode Task kids):ts) = makeTupleTask t : getGlobals ts
getGlobals (t:ts) = getGlobals ts
        
-- type and correctness checking of expression sub trees
getAndCheckExpr :: [(String, String)] -> AST -> String
getAndCheckExpr varList node = case node of
        (ASTNode Idf [ASTLeaf var])                                 -> getType varList node
        (ASTNode Idf [ASTLeaf var, i]) 
            | isPrefixOf "ERR" expected                             -> expected
            | notArray                                              -> ("ERR" ++ show var ++ "is not an array")
            | otherwise                                             -> elemType
            where
                expected    = getType varList node
                notArray    = not (isPrefixOf "TypeArray" expected)
                elemType    = Data.Maybe.fromJust (stripPrefix "TypeArray" expected)
        (ASTNode Array elements)
            | typeErrs /= []                                        -> typeErrs !! 0
            | length elemTypes == 0                                 -> "TypeEmpty"
            | allSameType                                           -> "TypeArray" ++ elemTypes!!0
            | otherwise                                             -> "ERRall elements in an array should be the same type"
            where
                elemTypes = map (getType varList) elements
                typeErrs = filter (isPrefixOf "ERR") elemTypes
                allSameType = and $ map (== head elemTypes) (tail elemTypes)
        (ASTNode Value [ASTNode t kids]) 
            | show t == "Boolean"                                   -> "TypeBool"
            | show t == "Integer"                                   -> "TypeInt"
            | otherwise                                             -> error ("type not recognized at getAndCheckExpr: " ++ show t)
        (ASTNode FuncCall (name@(ASTNode FuncName [ASTLeaf n]):args))
            | typErr                                                -> typ
            | argsOk                                                -> returnType
            | otherwise                                             -> ("ERRTask " ++ n ++ " takes arguments of type " ++ show argTypes0 ++", not of type " ++ show argTypes1)
            where
                typ             = getType varList node
                typErr          = isPrefixOf "ERR" typ
                txtType         = TXT.pack(typ)
                txtTypeSplit    = TXT.splitOn (TXT.pack ",") txtType
                typeSplit       = map TXT.unpack txtTypeSplit
                returnType      = head typeSplit
                argTypes0       = tail typeSplit
                argTypes1       = map (getAndCheckExpr varList) args
                argTuples       = zip argTypes0 argTypes1
                argsOk          = length argTypes0 == length argTypes1 && not (False `elem` (map (\(x,y) -> x == y) argTuples))
                
        (ASTNode Expr [x])                                          -> getAndCheckExpr varList x
        (ASTNode Expr [left, ASTNode Op [ASTLeaf op], right]) 
            | op == "equals" && rightT == leftT                     -> "TypeBool"
            | op == "equals"                                        -> "ERRoperator 'equals' takes an expression of the same type on each side"
            | isBoolOp && leftT == rightT && leftT == "TypeBool"    -> "TypeBool"
            | isIntOp && leftT == rightT && leftT == "TypeInt"      -> "TypeInt"
            | isBoolOp                                              -> ("ERR" ++ (show op) ++ " takes a boolean on each side.")
            | isIntOp                                               -> ("ERR" ++ (show op) ++ " takes an integer on each side.")
            | isIntBoolOp && leftT == rightT && leftT == "TypeInt"  -> "TypeBool"
            | isIntBoolOp                                           -> ("ERR" ++ op ++ " takes an integer on each side")
            | otherwise                                             -> ("ERRunknown operator" ++ show op)
            where
                leftT       = getAndCheckExpr varList left
                rightT      = getAndCheckExpr varList right
                isBoolOp    = op `elem` ["and", "or"] 
                isIntOp     = op `elem` ["plus", "minus", "times", "DividedBy"]
                isIntBoolOp = op `elem` ["GreaterThan", "GreaterThanEq", "SmallerThan", "SmallerThanEq"]
        t                                                           -> error ("error at getAndCheckExpr --> " ++ show t)

-- get type of variable from varList
getType :: [(String,String)] -> AST -> String
getType [] (ASTNode Idf [ASTLeaf var])              = ("ERRVariable " ++ var ++ " not in scope")
getType varList (ASTNode Idf [ASTLeaf var, i])      = getType varList (ASTNode Idf [ASTLeaf var])
getType ((n,t):xs) t2@(ASTNode Idf [ASTLeaf var]) 
        | n == var                                  = t
        | otherwise                                 = getType xs t2
getType _ (ASTNode Value [ASTNode Integer _])       = "TypeInt"
getType _ (ASTNode Value [ASTNode Boolean _])       = "TypeBool"
getType [] t2@(ASTNode FuncCall (ASTNode FuncName [ASTLeaf name]:args)) = ("ERRFunction " ++ name ++ " not in scope")
getType ((n,t):xs) t2@(ASTNode FuncCall (ASTNode FuncName [ASTLeaf name]:args))
        | n == name                                 = t
        | otherwise                                 = getType xs t2
getType _ t                                         = error ("unsupported node in getType --> " ++ show t)

-- check if a variable is already defined in the same scope (scopes are split by a special tuple (#,#))
inSameScope :: [(String,String)] -> String -> Bool
inSameScope [] _ = False
inSameScope ((n,t):xs) varname
        | n == varname = True
        | t == "#" = False
        | otherwise = inSameScope xs varname
        
-- make tuple to add to varList
makeTupleTask :: AST -> (String, String)
makeTupleTask (ASTNode Task kids) = makeTupleTaskH kids ("", "")
makeTupleTask t = error ("Node not supported in makeTupleTask --> " ++ show t)

makeTupleTaskH :: [AST] -> (String, String) -> (String, String)
makeTupleTaskH [] tuple = tuple
makeTupleTaskH (ASTNode FuncName [ASTLeaf name]:ts) (n,tp) = makeTupleTaskH ts (name, tp)
makeTupleTaskH (ASTNode Args kids:ts) tuple = makeTupleTaskH ts (makeTupleTaskH kids tuple)
makeTupleTaskH (ASTNode Arg [ASTNode Type [ASTLeaf t], _]:ts) (n,tp) = makeTupleTaskH ts (n,tp ++ "," ++ t)
makeTupleTaskH (ASTNode Type [ASTLeaf t]:ts) (n,tp) = makeTupleTaskH ts (n,t ++ tp)
makeTupleTaskH (ASTNode Body kids:ts) (n,tp) = r
        where
            actualR = getReturnType kids
            expectedR = TXT.unpack ((TXT.splitOn (TXT.pack ",") (TXT.pack tp))!!0)
            ok = actualR == expectedR
            r = do if ok then makeTupleTaskH ts (n,tp) else if isPrefixOf "ERR" actualR then ("ERR", drop 3 actualR) else ("ERR", "Task " ++ n ++ " should return a " ++ expectedR ++ ", but returns a " ++ actualR)
makeTupleTaskH (t:ts) tuple = error ("Node not supported in makeTupleTaskH --> " ++ show t)

makeTupleDecl :: [(String,String)] -> AST -> (String, String)
makeTupleDecl varList (ASTNode _ [_, ASTNode Idf [ASTLeaf nameStr]]) | inSameScope varList nameStr = ("ERR","variable " ++ nameStr ++ " is already defined in this scope")
makeTupleDecl _ (ASTNode _ [ASTNode Type [ASTLeaf typeStr], ASTNode Idf [ASTLeaf nameStr]]) = (nameStr, typeStr)
makeTupleDecl _ (ASTNode Decl [ASTNode Type [ASTNode TypeArray [ASTNode Type [ASTLeaf typestr]]], ASTNode Idf [ASTLeaf nameStr]]) = (nameStr, (show TypeArray) ++ typestr)
makeTupleDecl _ t = error ("Node not supported in makeTupleDecl --> " ++ show t)

getReturnType :: [AST] -> String -- kids of the body of the task should go in here only
getReturnType [] = "ERRreturnVariable not declared in the task"
getReturnType ts | getNodeType (head (reverse ts)) /= Idf = "TypeNothing"
getReturnType (ASTNode Decl [ASTNode Type [ASTLeaf t], idf]:ts)
        | idf == head (reverse ts) = t 
        | otherwise = getReturnType ts
getReturnType (ASTNode Decl [ASTNode Type [ASTLeaf t], idf, _]:ts)
        | idf == head (reverse ts) = t 
        | otherwise = getReturnType ts
getReturnType (t:ts) = getReturnType ts -- not a decl

getNodeType :: AST -> Alphabet
getNodeType (ASTNode x _) = x

-- get line nr for decent error throwing
getPath :: AST -> AST -> [Int] -> [Int]
getPath t@(ASTLeaf _) t2 path 
        | t == t2 = path
        | otherwise = []
getPath t@(ASTNode _ []) t2 path = []
getPath t@(ASTNode _ kids) t2 path 
        | t == t2 = path
        | pl == [] = []
        | otherwise = pl !! 0
        where
            pl = filter (\x -> x /= []) [getPath (kids!!i) t2 (path ++ [i]) | i <- [0..((length kids)-1)]]

-- counts lines that are in front/above a certain node
countLinesAbove :: AST -> [Int] -> Int
countLinesAbove t [] = 0
countLinesAbove (ASTNode _ kids) (x:xs) = (sum (map countLines (take (x-1) kids)) ) + 1 + countLinesAbove (kids!!x) xs

-- counts how many lines of codes a node contains
countLines :: AST -> Int
countLines (ASTLeaf _) = 0
countLines (ASTNode Decl _) = 1
countLines (ASTNode Assign _) = 1 
countLines (ASTNode FuncCall _) = 1
countLines (ASTNode While kids) = sum (map countLines kids) 
countLines (ASTNode When kids) = sum (map countLines kids) 
countLines (ASTNode _ kids) = sum (map countLines kids)

-- gives the line number corresponding to a certain node
-- the first argument should be the root of the ast (the Program node)
getLineNr :: AST -> AST -> Int
getLineNr root t = countLinesAbove root (getPath root t [])