{-# LANGUAGE FlexibleInstances #-}

module Parse where


import Debug.Trace
import Data.List
import FPPrac.Trees
import Prelude
import Data.Char
import qualified Data.Text as TXT


-- Embedded language for alphabet: the first 10 clauses should not be removed, the last three can be replaced by your own.
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

                -- A few non-terminals as example; to be filled in for your own language
                | Program                       
                | ProgBody                       
                | Decl                     
                | Assign
                | When
                | While
                | Task                           
                | Arg
                | FuncName
                | Body
                | ProgLine
                | Line
                | MComp
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
                | Boolean
                | TypeBool
                | Integer
                | TypeInt
                | Character
                | TypeChar
                | TrueK
                | FalseK
                | Comp
                | CompOp
                | Incr
                | Error

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
-- Example grammar, to illustrate the structure of the definition of the grammar as a function

type Grammar = Alphabet -> [[Alphabet]]

grammar :: Grammar

grammar nt = case nt of

        Program -> [[prog, FuncName, ProgBody]]
        
        ProgBody -> [[semi, Rep0 [ProgLine], stop, dot]]
        
        ProgLine -> [[Task]
                    ,[Line]]
        
        Line    -> [[Decl]
                    ,[Assign]
                    ,[When]
                    ,[While]
                    ,[Incr]]

        Decl    -> [[suppose, Opt [global], Type, Idf, is, Value, dot]
                    ,[suppose, Opt [global], Type, Idf, dot]]
                    
        Assign  -> [[Idf, is, Value, dot]
                    ,[Idf, is, Expr, dot]]
                    
        Incr    -> [[inc, Idf, dot]]
        
        When    -> [[when, Comp, doK, Body]
                    ,[when, Comp, doK, Body, otherwiseK, doK, Body]]
                           
        While   -> [[while, Comp, doK, Body]]
                   
        Task    -> [[task, FuncName, takes, Rep0[Arg], gives, Type, after, Body, give, Alt [Value] [Idf], dot]]
        
        Arg     -> [[Type, Idf, Alt [comma] [andK]]]
        
        FuncName -> [[funcName]]
        
        Body    -> [[semi, Rep0 [Line], stop, dot]]
        
        
        
        Comp    -> [[Expr, MComp]]
        
        MComp   -> [[CompOp, MComp, MComp]
                    ,[CompOp, MComp]
                    ,[Expr]]
                    
        Expr    -> [[Value, ExprH]
                    ,[Value]
                    ,[Idf, ExprH]
                    ,[Idf]]
                    
        ExprH   -> [[Op, Alt [Value] [Idf], ExprH]
                    ,[Op, Alt [Value] [Idf]]]
                    
        Op      -> [[plus]
                    ,[minus]
                    ,[times]
                    ,[divided, by]]
        
        CompOp  -> [[equals]
                    ,[andK]
                    ,[orK]
                    ,[is]
                    ,[GreaterThan]
                    ,[GreaterThanEq]
                    ,[SmallerThan]
                    ,[SmallerThanEq]]
                    
        GreaterThan -> [[is, greater, than]]
        GreaterThanEq -> [[is, greater, than, orK, equal, to]]
        SmallerThan -> [[is, smaller, than]]
        SmallerThanEq -> [[is, smaller, than, orK, equal, to]]
        
        Type    -> [[TypeBool]
                   ,[TypeInt]
                   ,[TypeChar]]
        
        Idf     -> [[idf]]
                   
        Value   -> [[Boolean]
                  -- ,[FalseK]
                   ,[int]
                   ,[char]]
                
        Boolean -> [[Alt [TrueK] [FalseK]]]
        TrueK   -> [[trueK]]
        FalseK  -> [[falseK]]
        TypeBool -> [[typeBool]]
        
        Integer -> [[int]]
        TypeInt -> [[typeInt]]
        
        Character -> [[char]]
        TypeChar -> [[typeChar]]
        

-- shorthand names can be handy, such as:
typeBool  = Keyword "boolean"
typeInt   = Keyword "integer"
typeChar  = Keyword "character"

bool      = SyntCat Boolean
trueK     = SyntCat TrueK
falseK    = SyntCat FalseK
int       = SyntCat Integer
char      = SyntCat Character
idf       = SyntCat Idf
funcName  = SyntCat FuncName

suppose     = Keyword "suppose"
after       = Keyword "after"
is          = Keyword "is"
equals      = Keyword "equals"

task        = Keyword "task"
global      = Keyword "global"
takes       = Keyword "takes"
comma       = Keyword ","
andK        = Keyword "and"
gives       = Keyword "gives"
dot         = Keyword "."
to          = Keyword "to"
while       = Keyword "while"
isK         = Keyword "is"
doK         = Keyword "do"
inc         = Keyword "increment"
plus        = Keyword "plus"
minus       = Keyword "minus"
times       = Keyword "times"
divided     = Keyword "divided"
by          = Keyword "by"
comment     = Keyword "btw"
when        = Keyword "when"
otherwiseK  = Keyword "otherwise"
nothing     = Keyword "nothing"
give        = Keyword "give"
greater     = Keyword "greater"
orK         = Keyword "or"
than        = Keyword "than"
equal       = Keyword "equal"
smaller     = Keyword "smaller"
stop        = Keyword "stop"
semi        = Keyword ":"
prog        = Keyword "program"


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
                        | otherwise     = error (show tokens)
        where
          ptrees = [ t  | r <- gr s
                        , (t,rem) <- parserGen gr r (s,[],tokens)
                        , rem == []
                        ]

-- ==================================================
-- Testing

-- Informal expression: suppose boolean a is false.

-- Corresponding tokenlist:
tokenlist = tok ": suppose integer b. btw, this is a comment. suppose integer b. stop."
tokenlist2 = [ (Keyword "task","task") , (FuncName,"f") , (Keyword "takes","takes") , 
    (TypeBool,"boolean"), (Idf,"a") , (Keyword ",", ","),
    (TypeInt,"integer"), (Idf,"b"), (Keyword "and","and"),
    (TypeChar,"character"), (Idf,"c"), (Keyword "and","and"), 
    (Keyword "gives","gives"), (TypeInt,"integer"), (Keyword "after:","after:"), (Keyword "suppose","suppose") , (TypeBool,"boolean") , (Idf,"a") , (Keyword "is","is") , (Boolean,"false"), (Keyword ".",".")]
tokenlist3 = tok ("task F takes boolean g, integer i and integer j and gives integer after: suppose integer b. btw, this is a comment. suppose integer b. stop.")
    -- ++ "suppose integer c."
   -- ++ "5 to a."
   -- ++ "10 to b."
   -- ++ "a + b to c.")
programma1 = tok ("program Test:"
    ++ "suppose integer a."
    ++"task F takes boolean g, integer i and integer j and gives integer after:"
        ++"suppose integer b. btw, this is a comment."
        ++"suppose integer c."
        ++"a is 5."
        ++"b is 10."
        ++"c is a plus b."
        ++"suppose integer h."
        ++"h is false."
        ++"while h is btw, this is also a comment. false do:"
            ++"increment b."
            ++"when b is greater than 20 and 1 equals 1 do:"
                ++"h is true."
                ++"stop."
            ++"stop."
        ++"when g is true do: btw, this is the 'if'."
            ++"a is a plus 1."
            ++"a is a times b."
            ++"increment a."
            ++"stop."
        ++"otherwise do:"
            ++"nothing "
            ++"stop."
        ++"stop."
        ++"give a."
    ++"suppose integer x."
    ++"stop.")

-- test0 calculates the parse tree:
test0 = parse grammar Body tokenlist
test1 = parse grammar Task tokenlist3
test2 = parse grammar Program programma1


-- For graphical representation, two variants of a toRoseTree function. Define your own to get a good view of the parsetree.
-- First open standard_webpage.html
toRoseTree0, toRoseTree1 :: ParseTree -> RoseTree

toRoseTree0 t = case t of
        PLeaf (c,s)     -> RoseNode "PLeaf" [RoseNode ("(" ++ show c ++ "," ++ s ++ ")") []]
        PNode nt ts     -> RoseNode "PNode" (RoseNode (show nt) [] : map toRoseTree0 ts)

test10 = showRoseTree $ toRoseTree0 test2

-- ---
toRoseTree1 t = case t of
        PLeaf (c,s)     -> RoseNode (show c) [RoseNode s []]
        PNode nt ts     -> RoseNode (show nt) (map toRoseTree1 ts)

test11 = showRoseTree $ toRoseTree1 test2

-- ============================================
-- building the AST

data AST = ASTNode Alphabet [AST] | ASTLeaf String deriving (Show, Eq)

toAST :: ParseTree -> AST
toAST (PLeaf (c,s)) = ASTLeaf s
toAST (PNode Line [t]) = toAST t -- this should be skipped
toAST (PNode ProgLine [t]) = toAST t
toAST (PNode FalseK [t]) = toAST t
toAST (PNode TrueK [t]) = toAST t
toAST (PNode TypeInt ts) = ASTLeaf (show TypeInt) -- make leaf of this
toAST (PNode TypeBool ts) = ASTLeaf (show TypeBool)
toAST (PNode TypeChar ts) = ASTLeaf (show TypeChar)
toAST (PNode GreaterThan ts) = ASTLeaf (show GreaterThan)
toAST (PNode GreaterThanEq ts) = ASTLeaf (show GreaterThanEq)
toAST (PNode SmallerThan ts) = ASTLeaf (show SmallerThan)
toAST (PNode SmallerThanEq ts) = ASTLeaf (show SmallerThanEq)
toAST (PNode Arg ts) = ASTNode Arg (map toAST ts2) where ts2 = [t | t <- ts, isPNode t] -- this one doenst need its leafs
toAST (PNode Task ts) = ASTNode Task (map toAST ts2) where ts2 = [t | t <- ts, isPNode t]
toAST (PNode Body ts) = ASTNode Body (map toAST ts2) where ts2 = [t | t <- ts, isPNode t]
toAST (PNode Decl ts) = ASTNode Decl (map toAST ts2) where ts2 = [t | t <- ts, isPNode t]
toAST (PNode When ts) = ASTNode When (map toAST ts2) where ts2 = [t | t <- ts, isPNode t]
toAST (PNode Program ts) = ASTNode Program (map toAST ts2) where ts2 = [t | t <- ts, isPNode t]
toAST (PNode Assign ts) = ASTNode Assign (map toAST ts2) where ts2 = [t | t <- ts, isPNode t]
toAST (PNode ProgBody ts) = ASTNode ProgBody (map toAST ts2) where ts2 = [t | t <- ts, isPNode t]
toAST (PNode While ts) = ASTNode While (map toAST ts2) where ts2 = [t | t <- ts, isPNode t]
toAST (PNode Incr ts) = ASTNode Incr (map toAST ts2) where ts2 = [t | t <- ts, isPNode t]
toAST (PNode nt ts) = ASTNode nt (map toAST ts)

-- showing the AST
astToRose :: AST -> RoseTree
astToRose (ASTLeaf s) = RoseNode s []
astToRose (ASTNode a asts) = RoseNode (show a) (map astToRose asts)

isPNode :: ParseTree -> Bool
isPNode (PNode _ _) = True
isPNode x = False

test12 = showRoseTree $ astToRose $ toAST test2

-- =========================================================
-- type checking
typeCheck :: AST -> [(String, Type)] -> Bool -- list of tuples (varName, varType), must be empty on call
typeCheck (ASTNode nt ts) varList = True

-- ==================================================
-- Clearly, you have to define your own embedded language for constrcuctions in your programming language.
--      This will naturally be a recursive algebraic type, such that it effectively represents the AST.
-- Besides, you'll have to transform a parsetree to a tree of this AST type.
-- Finally, the bck-end of the compiler (code-generation) will be a function of an AST into a list of SprIL instructions.
--
-- Just a hint: use pattern matching on trees

-- ========================================================
-- tokenizer

-- error ("Dear sir,\n We have noticed that you have made a mistake in your beloved code. You have used a word that we do not recognize, namely" ++ x ++ ". We kindly ask you to reconsider your code.\n Thanks in advance, we hope we can help you further next time.\n Good luck!")

--data T = Error | Suppose | Intt | Boolt | Chart | Intval | Truet | Falset | Charval | Task | Fname | Takes | Comma | And | Gives | Btw | Dot | To | While | Is | Do | Incr | Plus | Minus | Times | Devides | When | Otherwise | Give | Nothingt | After | Varname | Greater | Or | Than | Smaller | Equals | Equal deriving (Show, Eq)

keys = ["suppose", "integer", "boolean", "character", "task", "takes", ",", "and", "gives", ".", "to", "while", "is", "do", "increment", "plus", "minus", "times", "divided", "by", "when", "otherwise", "give", "after", "greater", "or", "than", "smaller", "equals", "equal", "stop", ":", "program"]
keyTokens = [suppose, typeInt, typeBool, typeChar, task, takes, comma, andK, gives, dot, to, while, is, doK, inc, plus, minus, times, divided, by, when, otherwiseK, give, after, greater, orK, than, smaller, equals, equal, stop, semi, prog]

tok :: String -> [(Alphabet,String)]
tok str = tokH (prepare str)

tokH :: [String] -> [(Alphabet,String)]
tokH [] = []
tokH (x:xs) | x == "" = tokH xs
            | isUpper (head x) = (FuncName, x) : tokH xs
            | x == "true" = (TrueK, x)  : tokH xs
            | x == "false" = (FalseK, x) : tokH xs
            | not (False `elem` (map isDigit x)) = (Integer, x) : tokH xs
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
fixdots txt = c
                where
                    a = TXT.replace (TXT.pack ",") (TXT.pack " , ") txt
                    b = TXT.replace (TXT.pack ".") (TXT.pack " . ") a
                    c = TXT.replace (TXT.pack ":") (TXT.pack " : ") b

removeComments :: [String] -> Bool -> [String]
removeComments [] b = []
removeComments (x:xs) True | x == "btw" = removeComments xs False
                            | x == "nothing" = removeComments xs True
                            | otherwise = x : removeComments xs True
removeComments (x:xs) False | x == "." = removeComments xs True
                            | otherwise = removeComments xs False