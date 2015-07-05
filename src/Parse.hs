{-# LANGUAGE FlexibleInstances #-}
module Parse where

import FPPrac.Trees
import DataTypesEtc
import Data.Char
import Grammar
import qualified Data.Maybe
import qualified Data.Text as TXT

-- main function to call here. input should be a complete program that you want parsed. it will return an AST that the type checker can use.
parse0 :: String -> AST
parse0 str = toAST $ parse grammar Program (tok str)

-- =================================================================
-- Parser generator
--
-- format:
-- parserGen gr rule (nt,ts,tokens)     - gr    : is the grammar
--                                      - rule  : is the rule which is now parsed
--                                      - nt    : is the Non-Terminal immediately above the sequence of subtrees under development
--                                      - ts    : is the sequence of trees already found, and which will be joined under nt
--                                      - tokens: the remaining sequence of tokens still to be parsed (of the form (Alphabet,String)).

parserGen :: Grammar -> [Alphabet] -> ParseState -> [(ParseTree,[Token])]

parserGen gr [] (nt0,ts,tokens) = [(PNode nt0 ts, tokens)]

parserGen _  _  ( _ , _,  []  ) = []

parserGen gr (nt:rule) (nt0,ts,(cat,str):tokens)
 = case nt of
        Symbol str'     ->  (if (str==str')
                                then -- traceShow ("success: " ++ str)
                                     (parserGen gr rule (nt0,ts,tokens))
                                else -- traceShow ("expected: " ++ str' ++ " -- found: " ++ str)
                                     []
                                )

        Keyword str'    ->  (if (str==str')
                                then -- traceShow ("success: " ++ str)
                                     (parserGen gr rule (nt0, ts++[PLeaf (cat,str)], tokens))
                                else -- traceShow ("expected: " ++ str' ++ " -- found: " ++ str)
                                     []
                                )

        SyntCat cat'    ->  (if (cat==cat')
                                then -- traceShow ("success: " ++ show cat ++ " " ++ str)
                                     (parserGen gr rule (nt0, ts++[PLeaf (cat,str)], tokens))
                                else -- traceShow ("expected: " ++ show cat' ++ " -- found: " ++ show cat ++ " " ++ str)
                                     []
                                )

        CheckToken p    ->  (if (p (cat,str))
                                then -- traceShow ("success: " ++ show cat ++ " " ++ str)
                                     (parserGen gr rule (nt0, ts++[PLeaf (cat,str)], tokens))
                                else -- traceShow ("expected: some property (...) -- found: " ++ show cat ++ " " ++ str)
                                     []
                                )

        Alt nts mts     ->     parserGen gr (nts++rule) (nt0,ts,(cat,str):tokens)
                            ++ parserGen gr (mts++rule) (nt0,ts,(cat,str):tokens)


        Try nts mts     ->  (if (parserGen gr nts (nt0,ts,(cat,str):tokens) /= [])
                                then (parserGen gr (nts++rule) (nt0,ts,(cat,str):tokens))
                                else (parserGen gr (mts++rule) (nt0,ts,(cat,str):tokens))
                                )

        Opt  nts        ->     parserGen gr (nts++rule) (nt0,ts,(cat,str):tokens)
                            ++ parserGen gr  rule       (nt0,ts,(cat,str):tokens)

        Rep0 nts        ->     parserGen gr (nts ++ (Rep0 nts : rule)) (nt0,ts,(cat,str):tokens)
                            ++ parserGen gr  rule                      (nt0,ts,(cat,str):tokens)

        Rep1 nts        ->     parserGen gr (nts ++ (Rep0 nts : rule)) (nt0,ts,(cat,str):tokens)




        _               ->     [  (t2,tokens2)  | r <- gr nt
                                                , (t1,tokens1) <- parserGen gr r (nt,[],(cat,str):tokens)
                                                , (t2,tokens2) <- parserGen gr rule (nt0,ts++[t1],tokens1)
                                                ]


-- ==================================================
-- parse:
--      Uses the parser generator function parseGen to produce only the parsetree (i.e., without rest-string).
--      Assumes a deterministic grammar and returns just the head of the list of all successful parse trees.
--
-- parse gr s tokens:   - gr    : grammar
--                      - s     : start symbol
--                      - tokens: tokenlist, as result of scanner/tokenizer

parse :: Grammar -> Alphabet -> [Token] -> ParseTree

parse gr s tokens       | ptrees /= []  = head ptrees
                        | otherwise     = error ("ParseError")
        where
          ptrees = [ t  | r <- gr s
                        , (t,rem) <- parserGen gr r (s,[],tokens)
                        , rem == []
                        ]


-- ==================================================
-- For graphical representation, two variants of a toRoseTree function. Define your own to get a good view of the parsetree.
-- First open standard_webpage.html
toRoseTree0, toRoseTree1 :: ParseTree -> RoseTree

toRoseTree0 t = case t of
        PLeaf (c,s)     -> RoseNode "PLeaf" [RoseNode ("(" ++ show c ++ "," ++ s ++ ")") []]
        PNode nt ts     -> RoseNode "PNode" (RoseNode (show nt) [] : map toRoseTree0 ts)

-- ---
toRoseTree1 t = case t of
        PLeaf (c,s)     -> RoseNode (show c) [RoseNode s []]
        PNode nt ts     -> RoseNode (show nt) (map toRoseTree1 ts)

-- ============================================
-- building the AST
-- ============================================
-- comment on the AST:
-- We should have defined a completely different AST, so that there is a different type of AST node 
-- for each possibility of what is in it and its sub trees (i.e. a type for declerations, a type for 
-- declerations with an assignment, a type for an assignment, and so on).
-- If we would have done that, it would be a lot easier to pattern match and also a lot more compact.
-- Sadly when we tought of this (remembered this) it was too late to change it.



toAST :: ParseTree -> AST
toAST node = case node of
        (PLeaf (c,s))                   -> ASTLeaf s
        (PNode Line [t])                -> toAST t -- these should be skipped
        (PNode ArrayVal [t, _])         -> toAST t
        (PNode ArrayVal [t])            -> toAST t
        (PNode FalseK [t])              -> toAST t
        (PNode VIA [t])                 -> toAST t
        (PNode TrueK [t])               -> toAST t -- 
        (PNode TypeInt ts)              -> ASTLeaf (show TypeInt) -- make leaf of these
        (PNode TypeBool ts)             -> ASTLeaf (show TypeBool)
        (PNode TypeChar ts)             -> ASTLeaf (show TypeChar)
        (PNode TypeNothing ts)          -> ASTLeaf (show TypeNothing)
        (PNode NotEqual ts)             -> ASTLeaf (show NotEqual)
        (PNode DividedBy ts)            -> ASTLeaf (show DividedBy)
        (PNode GreaterThan ts)          -> ASTLeaf (show GreaterThan)
        (PNode GreaterThanEq ts)        -> ASTLeaf (show GreaterThanEq)
        (PNode SmallerThan ts)          -> ASTLeaf (show SmallerThan)
        (PNode SmallerThanEq ts)        -> ASTLeaf (show SmallerThanEq) -- 
        (PNode Arg ts)                  -> ASTNode Arg (map toAST ts2) where ts2 = [t | t <- ts, isPNode t] -- these don't need its leafs
        (PNode Task ts)                 -> ASTNode Task (map toAST ts2) where ts2 = [t | t <- ts, isPNode t]
        (PNode Body ts)                 -> ASTNode Body (map toAST ts2) where ts2 = [t | t <- ts, isPNode t]
        (PNode Decl ts)                 -> ASTNode Decl (map toAST ts2) where ts2 = [t | t <- ts, isPNode t]
        (PNode When ts)                 -> ASTNode When (map toAST ts2) where ts2 = [t | t <- ts, isPNode t]
        (PNode Program ts)              -> ASTNode Program (map toAST ts2) where ts2 = [t | t <- ts, isPNode t]
        (PNode Assign ts)               -> ASTNode Assign (map toAST ts2) where ts2 = [t | t <- ts, isPNode t]
        (PNode ProgBody ts)             -> ASTNode ProgBody (map toAST ts2) where ts2 = [t | t <- ts, isPNode t]
        (PNode While ts)                -> ASTNode While (map toAST ts2) where ts2 = [t | t <- ts, isPNode t]
        (PNode Incr ts)                 -> ASTNode Incr (map toAST ts2) where ts2 = [t | t <- ts, isPNode t]
        (PNode FuncCall ts)             -> ASTNode FuncCall (map toAST ts2) where ts2 = [t | t <- ts, isPNode t] --
        (PNode nt ts)                   -> ASTNode nt (map toAST ts) -- do nothing special, just direct to the sub trees

-- showing the AST
showAST :: AST -> IO()
showAST t = showRoseTree $ astToRose t

astToRose :: AST -> RoseTree
astToRose (ASTLeaf s) = RoseNode s []
astToRose (ASTNode a asts) = RoseNode (show a) (map astToRose asts)

isPNode :: ParseTree -> Bool
isPNode (PNode _ _) = True
isPNode x = False


-- ==================================================
-- tokenizer
-- ==================================================
-- list of tokens in text form
keys = ["suppose", "integer", "boolean", "character", "task", "takes", ",", "and", "gives", 
        ".", "to", "while", "is", "do", "increment", "plus", "minus", "times", "divided", 
        "by", "when", "otherwise", "give", "after", "greater", "or", "than", "smaller", 
        "equals", "equal", "stop", ":", "program", "(", ")", "[", "]", "of", "length"]
-- list of tokens in Alphabet form
keyTokens = [suppose, typeInt, typeBool, typeChar, task, takes, comma, andK, gives, dot, to, 
        while, is, doK, inc, plus, minus, times, divided, by, when, otherwiseK, give, after, 
        greater, orK, than, smaller, equals, equal, stop, semi, prog, lPar, rPar, lBracket, 
        rBracket, ofK, lengthK]
-- these lists have the same index for the same token so if a string is equal to anything in the first list,
-- the keyword is the element in the second list at the same index.

-- takes a full program as a string and returns a list of tokens ready to be parsed.
-- first prepares the string, then puts it in the helper function
tok :: String -> [(Alphabet,String)]
tok str = tokH (prepare str)

-- gets a list of strings, made from the program string and returns a list of tokens.
tokH :: [String] -> [(Alphabet,String)]
tokH [] = []
tokH (x:xs) | x == "" = tokH xs
            | isUpper (head x) = (FuncName, x) : tokH xs
            | x == "true" = (TrueK, x)  : tokH xs
            | x == "false" = (FalseK, x) : tokH xs
            | not (False `elem` (map (\y -> isDigit y || y =='-') x)) = (Integer, x) : tokH xs
            | head x == '\'' && head (reverse x) == '\'' && length x == 3 = (Character, x) : tokH xs
            | a /= Error = (a,b) : tokH xs
            | otherwise = (Idf, x) : tokH xs
            where
                (a,b) = getToken x
       
-- checks if it is a token using the keys list and which one using also the keytokens list
-- getToken just makes sure getTokenH has the keys and keytokens list it needs.
getToken :: String -> (Alphabet,String)       
getToken str = getTokenH keys keyTokens str
getTokenH :: [String] -> [Alphabet] -> String -> (Alphabet,String)
getTokenH [] [] s = (Error, "")
getTokenH (x:xs) (y:ys) s | s == x = (y,x)
                          | otherwise = getTokenH xs ys s
                          
-- prepares the program string to be parsed.
-- first calls fix dots, then splits the string on spaces so we have separate words.
prepare :: String -> [String]                          
prepare str = strlist
            where
                txt = TXT.pack str
                txt2 = fixdots txt
                strl = (map TXT.unpack (TXT.splitOn (TXT.pack " ") txt2))
                strlist = removeComments strl True

-- puts spaces around several characters so it will be recognized as a token                
fixdots :: TXT.Text -> TXT.Text
fixdots txt = h
            where
                a = TXT.replace (TXT.pack ",") (TXT.pack " , ") txt
                b = TXT.replace (TXT.pack ".") (TXT.pack " . ") a
                c = TXT.replace (TXT.pack ":") (TXT.pack " : ") b
                d = TXT.replace (TXT.pack "(") (TXT.pack " ( ") c
                e = TXT.replace (TXT.pack ")") (TXT.pack " ) ") d
                f = TXT.replace (TXT.pack "[") (TXT.pack " [ ") e
                g = TXT.replace (TXT.pack "]") (TXT.pack " ] ") f
                h = TXT.replace (TXT.pack "\n") (TXT.pack "") g

-- removes the comments; iterates over the string list, the boolean is whether we should be taking words or not.
-- if we see the word  "btw" we stop taking and just skip the words until we see a dot.
removeComments :: [String] -> Bool -> [String]
removeComments [] b = []
removeComments (x:xs) True  | x == "btw" = removeComments xs False
                            | otherwise = x : removeComments xs True
removeComments (x:xs) False | x == "." = removeComments xs True
                            | otherwise = removeComments xs False