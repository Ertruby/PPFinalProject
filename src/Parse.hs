{-# LANGUAGE FlexibleInstances #-}

module Parse where


import Debug.Trace
import Data.List
import FPPrac.Trees
import Prelude
import Data.Char
import qualified Data.Maybe
import qualified Data.Text as TXT


parse0 :: String -> AST
parse0 str = toAST $ parse grammar Program (tok str)

-- Embedded language for alphabet:
data Alphabet =   Symbol     String             -- Token given ("char" specific for this example)
                | Keyword    String             -- A given string, but included in the parsetree
                | SyntCat    Alphabet           -- A given string, but included in the parsetree
                | CheckChar  (Char->Bool)       -- Character should have some property (for tokenizer)
                | CheckToken (Token->Bool)      -- Token should have some property (for parser)

                | Alt   [Alphabet] [Alphabet]   -- Try both
                | Try   [Alphabet] [Alphabet]   -- If first doesn't work, try second
                | Opt   [Alphabet]              -- Optional
                | Rep0  [Alphabet]              -- Zero or more repetitions
                | Rep1  [Alphabet]              -- One or more repetitions

                -- Non-terminals
                | Program                       
                | ProgBody                       
                | Decl                     
                | Assign
                | When
                | While
                | Task                           
                | Arg
                | Args
                | FuncName
                | Body
                | ProgLine
                | Line
                | Expr
                | ExprH
                | Op
                | GreaterThan
                | GreaterThanEq
                | SmallerThan
                | SmallerThanEq
                | Type
                | Idf
                | Value
                | Array
                | ArrayVal
                | TypeArray
                | Boolean
                | TypeBool
                | Integer
                | TypeInt
                | Character
                | TypeChar
                | TrueK
                | FalseK
                | Incr
                | VIA
                | DividedBy
                | FuncCall
                | Error
                | TypeNothing
                deriving (Eq,Show)


-- functions for shorthand notation for EBNF constructions
ps <>  qs = Alt  ps qs
ps <<> qs = Try  ps qs
(?>) ps   = Opt  ps
(*>) ps   = Rep0 ps
(+>) ps   = Rep1 ps

-- "Hack"
instance Eq   (Char  -> Bool) where f == g = True       -- These instances are a hack, since otherwise "deriving (Eq,Show)"
instance Eq   (Token -> Bool) where f == g = True       --      for Alphabet won't work.
instance Show (Char  -> Bool) where show f = ""         -- Just make sure you never apply (==) or show to function types.
instance Show (Token -> Bool) where show f = ""

-- ==========================================================================================================

type Grammar = Alphabet -> [[Alphabet]]

grammar :: Grammar

grammar nt = case nt of

        Program -> [[prog, FuncName, ProgBody]]
        
        ProgBody    -> [[semi, Rep0 [ProgLine], stop, dot]]
        
        ProgLine    -> [[Alt [Task] [Line]]]
        
        Line    -> [[Decl]
                    ,[Assign]
                    ,[FuncCall]
                    ,[Incr]
                    ,[When]
                    ,[While]]

        Decl    -> [[suppose, Opt [global], Type, Idf, Alt [ofK, lengthK, Expr] [Opt [is, Expr]], dot]]
                    
        Assign  -> [[Idf, is, Expr, dot]]
                    
        FuncCall    ->  [[FuncName, lPar, Rep0 [Expr, Opt [comma]], rPar, Opt [dot]]
                        ,[FuncName, lPar, Rep0 [Expr, Opt [comma]], rPar]]
                    
        Incr    -> [[inc, Idf, dot]]
        
        When    -> [[when, Expr, doK, Body, Opt [otherwiseK, doK, Body]]]
                           
        While   -> [[while, Expr, doK, Body]]
                   
        Task    -> [[task, FuncName, takes, Args, gives, Type, after, Body]]
        
        Args    -> [[Rep0[Arg]]]
        
        Arg     -> [[Type, Idf, Alt [comma] [andK]]]
        
        Body    -> [[semi, Rep0 [Line], Alt [stop, dot] [give, VIA, dot]]]
                    
        Expr    -> [[VIA, Op, Expr]
                    ,[lPar, Expr, rPar, Op, Expr]
                    ,[lPar, Expr, rPar]
                    ,[VIA]]
                    
        VIA     -> [[Value]
                    ,[FuncCall]
                    ,[Idf]
                    ,[Array]]
                    
        Op      -> [[plus]
                    ,[minus]
                    ,[times]
                    ,[DividedBy]
                    ,[equals]
                    ,[is]
                    ,[GreaterThan]
                    ,[GreaterThanEq]
                    ,[SmallerThan]
                    ,[SmallerThanEq]
                    ,[andK]
                    ,[orK]]
        
        DividedBy   -> [[divided, by]]
        GreaterThan -> [[is, greater, than]]
        GreaterThanEq -> [[is, greater, than, orK, equal, to]]
        SmallerThan -> [[is, smaller, than]]
        SmallerThanEq -> [[is, smaller, than, orK, equal, to]]
        
        FuncName -> [[funcName]]
        
        Type    -> [[TypeBool]
                   ,[TypeInt]
                   ,[TypeChar]
                   ,[TypeArray]
                   ,[TypeNothing]]
        
        Idf     -> [[idf, Opt [lBracket, Expr, rBracket]]]
                   
        Value   -> [[Boolean]
                   ,[Integer]
                   ,[Character]]
                   
        Array   -> [[lBracket, Rep0 [ArrayVal], rBracket]]
        ArrayVal    -> [[VIA, Opt [comma]]]
        TypeArray   -> [[lBracket, Type, rBracket]]
                
        Boolean -> [[Alt [TrueK] [FalseK]]]
        TrueK   -> [[trueK]]
        FalseK  -> [[falseK]]
        TypeBool -> [[typeBool]]
        
        Integer -> [[int]]
        TypeInt -> [[typeInt]]
        
        Character -> [[char]]
        TypeChar -> [[typeChar]]
                
        TypeNothing -> [[nothing]]
        

-- shorthand names can be handy, such as:
lPar        = Symbol "("
rPar        = Symbol ")"
lBracket    = Symbol "["
rBracket    = Symbol "]"

bool        = SyntCat Boolean
trueK       = SyntCat TrueK
falseK      = SyntCat FalseK
int         = SyntCat Integer
char        = SyntCat Character
idf         = SyntCat Idf
funcName    = SyntCat FuncName

typeBool    = Keyword "boolean"
typeInt     = Keyword "integer"
typeChar    = Keyword "character"
greater     = Keyword "greater"
orK         = Keyword "or"
than        = Keyword "than"
equal       = Keyword "equal"
smaller     = Keyword "smaller"
equals      = Keyword "equals"
inc         = Keyword "increment"
plus        = Keyword "plus"
minus       = Keyword "minus"
times       = Keyword "times"
divided     = Keyword "divided"
by          = Keyword "by"

suppose     = Keyword "suppose"
after       = Keyword "after"
is          = Keyword "is"
task        = Keyword "task"
global      = Keyword "global"
takes       = Keyword "takes"
comma       = Keyword ","
andK        = Keyword "and"
gives       = Keyword "gives"
dot         = Keyword "."
to          = Keyword "to"
while       = Keyword "while"
doK         = Keyword "do"
comment     = Keyword "btw"
when        = Keyword "when"
otherwiseK  = Keyword "otherwise"
nothing     = Keyword "nothing"
give        = Keyword "give"
stop        = Keyword "stop"
semi        = Keyword ":"
prog        = Keyword "program"
ofK         = Keyword "of"
lengthK     = Keyword "length"


-- ==========================================================================================================
-- Parsing      - Note that a ParseTree contains a lot of syntactic information that is not desirable in an AST.
--                So you still have to write a function that transforms a ParseTree into an AST.
-- Token        - a 2-tuple of a non-terminal and a string, where the non-terminal
--                indicates to what syntactic category teh string belongs.

type Token      = (Alphabet,String)

data ParseTree  = PLeaf Token                   -- PLeaf: ParseTree-Leaf
                | PNode Alphabet [ParseTree]    -- PNode: ParseTree-Node
                deriving (Eq,Show)

type ParseState = ( Alphabet                    -- Non-terminal indicating the present subexpression
                  , [ParseTree]                 -- The already produced trees within the present subexpression
                  , [Token]                     -- The remaining list of input tokens
                  )


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

data AST = ASTNode Alphabet [AST] | ASTLeaf String deriving (Show, Eq)

toAST :: ParseTree -> AST
toAST node = case node of
        (PLeaf (c,s))                   -> ASTLeaf s
        (PNode Line [t])                -> toAST t -- this should be skipped
        (PNode ProgLine [t])            -> toAST t
        (PNode ArrayVal [t, _])         -> toAST t
        (PNode ArrayVal [t])            -> toAST t
        (PNode FalseK [t])              -> toAST t
        (PNode VIA [t])                 -> toAST t
        (PNode TrueK [t])               -> toAST t
        (PNode TypeInt ts)              -> ASTLeaf (show TypeInt) -- make leaf of this
        (PNode TypeBool ts)             -> ASTLeaf (show TypeBool)
        (PNode TypeChar ts)             -> ASTLeaf (show TypeChar)
        (PNode TypeNothing ts)          -> ASTLeaf (show TypeNothing)
        (PNode DividedBy ts)            -> ASTLeaf (show DividedBy)
        (PNode GreaterThan ts)          -> ASTLeaf (show GreaterThan)
        (PNode GreaterThanEq ts)        -> ASTLeaf (show GreaterThanEq)
        (PNode SmallerThan ts)          -> ASTLeaf (show SmallerThan)
        (PNode SmallerThanEq ts)        -> ASTLeaf (show SmallerThanEq)
        (PNode Arg ts)                  -> ASTNode Arg (map toAST ts2) where ts2 = [t | t <- ts, isPNode t] -- this one doenst need its leafs
        (PNode Task ts)                 -> ASTNode Task (map toAST ts2) where ts2 = [t | t <- ts, isPNode t]
        (PNode Body ts)                 -> ASTNode Body (map toAST ts2) where ts2 = [t | t <- ts, isPNode t]
        (PNode Decl ts)                 -> ASTNode Decl (map toAST ts2) where ts2 = [t | t <- ts, isPNode t]
        (PNode When ts)                 -> ASTNode When (map toAST ts2) where ts2 = [t | t <- ts, isPNode t]
        (PNode Program ts)              -> ASTNode Program (map toAST ts2) where ts2 = [t | t <- ts, isPNode t]
        (PNode Assign ts)               -> ASTNode Assign (map toAST ts2) where ts2 = [t | t <- ts, isPNode t]
        (PNode ProgBody ts)             -> ASTNode ProgBody (map toAST ts2) where ts2 = [t | t <- ts, isPNode t]
        (PNode While ts)                -> ASTNode While (map toAST ts2) where ts2 = [t | t <- ts, isPNode t]
        (PNode Incr ts)                 -> ASTNode Incr (map toAST ts2) where ts2 = [t | t <- ts, isPNode t]
        (PNode FuncCall ts)             -> ASTNode FuncCall (map toAST ts2) where ts2 = [t | t <- ts, isPNode t]
        (PNode nt ts)                   -> ASTNode nt (map toAST ts)

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
keys = ["suppose", "integer", "boolean", "character", "task", "takes", ",", "and", "gives", 
        ".", "to", "while", "is", "do", "increment", "plus", "minus", "times", "divided", 
        "by", "when", "otherwise", "give", "after", "greater", "or", "than", "smaller", 
        "equals", "equal", "stop", ":", "program", "(", ")", "[", "]", "of", "length"]
keyTokens = [suppose, typeInt, typeBool, typeChar, task, takes, comma, andK, gives, dot, to, 
        while, is, doK, inc, plus, minus, times, divided, by, when, otherwiseK, give, after, 
        greater, orK, than, smaller, equals, equal, stop, semi, prog, lPar, rPar, lBracket, 
        rBracket, ofK, lengthK]

tok :: String -> [(Alphabet,String)]
tok str = tokH (prepare str)

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
       
getToken :: String -> (Alphabet,String)       
getToken str = getTokenH keys keyTokens str
getTokenH :: [String] -> [Alphabet] -> String -> (Alphabet,String)
getTokenH [] [] s = (Error, "")
getTokenH (x:xs) (y:ys) s | s == x = (y,x)
                          | otherwise = getTokenH xs ys s
prepare :: String -> [String]                          
prepare str = strlist
            where
                txt = TXT.pack str
                txt2 = fixdots txt
                strl = (map TXT.unpack (TXT.splitOn (TXT.pack " ") txt2))
                strlist = removeComments strl True
                    
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

removeComments :: [String] -> Bool -> [String]
removeComments [] b = []
removeComments (x:xs) True  | x == "btw" = removeComments xs False
                            | otherwise = x : removeComments xs True
removeComments (x:xs) False | x == "." = removeComments xs True
                            | otherwise = removeComments xs False