{-# LANGUAGE FlexibleInstances #-}

module Parse where


import Debug.Trace
import Data.List
import FPPrac.Trees
import Prelude
import Data.Char
import qualified Data.Maybe
import qualified Data.Text as TXT


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
-- Testing

-- Informal expression: suppose boolean a is false.

-- Corresponding tokenlist:
tokenlist = tok "when 5 is greater than 3 do: suppose integer b is (3 plus 4) times (5 minus 2). suppose boolean c is false. b is 5 times (6 minus 3). stop."
tkl = tok (": suppose boolean g is false."
            ++"suppose integer i."
            ++"when g do:"
                ++"g is true."
            ++"stop." 
            ++"otherwise do:"
                ++"i is 10."
            ++"stop."
            ++"while i is smaller than 7 do:"
                ++"increment i."
            ++"stop."
            ++"stop.")
tsk = tok (": task Fib takes integer n and gives integer after:"
                ++"suppose integer k."
                ++"when (n equals 0) or (n equals 1) do:"
                    ++"k is 1."
                    ++"stop."
                ++"otherwise do:"
                    ++"k is Fib(n minus 1) plus Fib(n minus 2)."
                    ++"stop."
                ++"give k."
            ++"suppose integer g is Fib(3)."
            ++"stop.")
tokenlist2 = tok ("((true and true) or (false and true))")
tokenlist3 = tok ("task F takes boolean g, integer i and integer j and gives integer after: suppose integer b. btw, this is a comment. suppose integer b. stop.")
programma1 = tok ("program Test:"
    ++"suppose integer a is 3."
    ++"task F takes boolean g, integer i and integer j and gives integer after:"
        ++"suppose integer b. btw, this is a comment."
        ++"suppose integer c."
        ++"a[3] is 5."
        ++"b is 10."
        ++"c is (a plus b) times 5."
        ++"a is 6."
        ++"suppose boolean g."
        ++"suppose boolean h."
        ++"g is false."
        ++"h is false."
        ++"while a is greater than b do:"
            ++"when (b is greater than 20) and (1 equals 1) do:"
                ++"h is true."
                ++"stop."
            ++"stop."
        ++"when g equals false do: btw, this is the 'if'."
            ++"a is a plus 1."
            ++"a is a times b."
            ++"increment a."
            ++"stop."
        ++"otherwise do:"
            ++"stop."
        ++"give a."
    ++"suppose integer x."
    ++"stop.")
    
programma2 = tok ("program Test:"
        ++"suppose [integer] a of length 3."
        ++"suppose integer herman is 5."
        ++"a is [5,-5,herman]."
        ++"suppose integer c is H(5)."
        ++"task Func takes boolean g, integer i and integer j and gives integer after:"
            ++"suppose integer b is 10."
            ++"suppose integer c is i plus j."
            ++"suppose [integer] o is [1,2,3]."
            ++"suppose [integer] k."
            ++"a[0] is 5."
            ++"a[1] is c."
            ++"c is a[0] plus b."
            ++"Stuff(5)."
            ++"g is false."
            ++"suppose boolean h is true."
            ++"while h do:"
                ++"increment b."
                ++"when b is greater than 20 do:"
                    ++"h is false."
                ++"stop."
            ++"stop."
            ++"when g equals false do:"
                ++"a[2] is a[0] times a[1]."
            ++"stop."
            ++"otherwise do:"
                ++"a[2] is b."
            ++"stop."
        ++"give c."
        ++"task Stuff takes integer i and gives nothing after:"
            ++"a[1] is 9."
        ++"stop."
        ++"task H takes integer i and gives integer after:"
            ++"suppose integer n is i plus 1."
        ++"give n."
    ++"stop.")

-- test0 calculates the parse tree:
test0 = parse grammar Body tkl
test1 = parse grammar ProgBody tsk
test2 = parse grammar Program programma1
test3 = parse grammar Program programma2

-- For graphical representation, two variants of a toRoseTree function. Define your own to get a good view of the parsetree.
-- First open standard_webpage.html
toRoseTree0, toRoseTree1 :: ParseTree -> RoseTree

toRoseTree0 t = case t of
        PLeaf (c,s)     -> RoseNode "PLeaf" [RoseNode ("(" ++ show c ++ "," ++ s ++ ")") []]
        PNode nt ts     -> RoseNode "PNode" (RoseNode (show nt) [] : map toRoseTree0 ts)

test10 = showRoseTree $ toRoseTree0 test1

-- ---
toRoseTree1 t = case t of
        PLeaf (c,s)     -> RoseNode (show c) [RoseNode s []]
        PNode nt ts     -> RoseNode (show nt) (map toRoseTree1 ts)

test11 = showRoseTree $ toRoseTree1 test1

-- ============================================
-- building the AST

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
astToRose :: AST -> RoseTree
astToRose (ASTLeaf s) = RoseNode s []
astToRose (ASTNode a asts) = RoseNode (show a) (map astToRose asts)

isPNode :: ParseTree -> Bool
isPNode (PNode _ _) = True
isPNode x = False

test12 = showRoseTree $ astToRose $ toAST test1

-- =========================================================
-- type checking.. also includes scope checking
-- =========================================================
test13 = typeCheckScope [toAST test3] []

-- main function
typeCheck :: AST -> AST -- supposed to take a Program node, works with more node types, but not all of them
typeCheck t | typeCheckScope [t] [] = t -- success
            | otherwise = error "error on typeCheck, typeCheckScope returned False. This should not be possible.."

-- general type checker per scope
typeCheckScope :: [AST] -> [(String, String)] -> Bool -- second argument (varList) should be empty when called by another function
typeCheckScope nodes varList = case nodes of
        []                                                                              -> True
        (t@(ASTNode Arg [_, _]):ts)                                                     -> typeCheckScope ts (makeTupleDecl varList t : varList)
        
        (t@(ASTNode Decl [_, _]):ts)                                                    -> typeCheckScope ts (makeTupleDecl varList t : varList)
        (t@(ASTNode Decl [tp@(ASTNode Type [ASTNode TypeArray kids0]), i, e@(ASTNode Expr [ASTNode Value kids])]):ts)
            | getAndCheckExpr varList (ASTNode Expr [ASTNode Value kids]) /= "TypeInt"  -> error ("length of array should be an integer --> " ++ show t) 
            | otherwise                                                                 -> typeCheckScope ts (makeTupleDecl varList (ASTNode Decl [tp,i]) : varList) 
        (t@(ASTNode Decl [tp, i, e]):ts)                                                -> typeCheckScope [ASTNode Assign [i,e]] newVarList && typeCheckScope ts newVarList where newVarList = makeTupleDecl varList (ASTNode Decl [tp,i]) : varList
        (t@(ASTNode Assign [t2@(ASTNode Idf [ASTLeaf var, i]), expr]):ts) 
            | iNotOke                                                                   -> error ("index of array should be an integer")
            | notArray                                                                  -> error (show var ++ "is not an array")
            | elemType == actual                                                        -> typeCheckScope ts varList
            | otherwise                                                                 -> error (show var ++ " is array of " ++ elemType ++ " not of " ++ actual ++ "." )
            where 
                iNotOke     = not (getAndCheckExpr varList i == "TypeInt")
                expected    = getType varList t2
                notArray    = not (isPrefixOf "TypeArray" expected)
                elemType    = Data.Maybe.fromJust (stripPrefix "TypeArray" expected)
                actual      = getAndCheckExpr varList expr
        (t@(ASTNode Assign [t2@(ASTNode Idf [ASTLeaf var]), expr]):ts)
            | expected == actual || actual == "TypeEmpty"                               -> typeCheckScope ts varList
            | otherwise                                                                 -> error (show var ++ " is of " ++ expected ++ " not " ++ actual ++ ". --> " ++ show t )
            where 
                expected    = getType varList t2
                actual      = getAndCheckExpr varList expr
        (t@(ASTNode Program kids):ts)                                                   -> typeCheckScope kids (("#", "#"):varList)
        (t@(ASTNode ProgBody kids):ts)                                                  -> typeCheckScope kids ((getGlobals kids)++varList) && typeCheckScope ts varList
        (t@(ASTNode Task kids):ts)                                                      -> typeCheckScope kids (("#", "#"):varList) && typeCheckScope ts varList
        (t@(ASTNode Body kids):ts)                                                      -> typeCheckScope kids varList && typeCheckScope ts varList
        (t@(ASTNode While [condition, body]):ts) 
            | (getAndCheckExpr varList condition) /= "TypeBool"                         -> error "While statement should contain a boolean expression"
            | otherwise                                                                 -> typeCheckScope [body] (("#", "#"):varList) && typeCheckScope ts varList
        (t@(ASTNode When kids):ts) 
            | (getAndCheckExpr varList (head kids)) /= "TypeBool"                       -> error "When statement should contain a boolean expression"
            | otherwise                                                                 -> typeCheckScope (tail kids) (("#", "#"):varList) && typeCheckScope ts varList
        (t@(ASTNode Incr [t2@(ASTNode Idf [ASTLeaf var])]):ts)
            | getType varList t2 /= "TypeInt"                                           -> error "increment takes an integer variable"
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
            | notArray                                              -> error (show var ++ "is not an array")
            | otherwise                                             -> elemType
            where
                expected    = getType varList node
                notArray    = not (isPrefixOf "TypeArray" expected)
                elemType    = Data.Maybe.fromJust (stripPrefix "TypeArray" expected)
        (ASTNode Array elements)                                     -> if length elemTypes == 0 then "TypeEmpty" else if allSameType then ("TypeArray" ++ elemTypes!!0) else error "all elements in an array should be the same type" 
            where
                elemTypes = map (getType varList) elements
                allSameType = and $ map (== head elemTypes) (tail elemTypes)
        (ASTNode Value [ASTNode t kids]) 
            | show t == "Boolean"                                   -> "TypeBool"
            | show t == "Integer"                                   -> "TypeInt"
            | otherwise                                             -> error ("type not recognized at getAndCheckExpr: " ++ show t)
        (ASTNode FuncCall (name:args))
            | argsOk                                                -> returnType
            | otherwise                                             -> error ("Function " ++ show name ++ " takes arguments of type " ++ show argTypes0 ++", not of type " ++ show argTypes1)
            where
                txtType         = TXT.pack(getType varList node)
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
            | op == "equals"                                        -> error "operator 'equals' takes an expression of the same type on each side"
            | isBoolOp && leftT == rightT && leftT == "TypeBool"    -> "TypeBool"
            | isIntOp && leftT == rightT && leftT == "TypeInt"      -> "TypeInt"
            | isBoolOp                                              -> error  ((show op) ++ " takes a boolean on each side.")
            | isIntOp                                               -> error  ((show op) ++ " takes an integer on each side.")
            | isIntBoolOp && leftT == rightT && leftT == "TypeInt"  -> "TypeBool"
            | isIntBoolOp                                           -> error (op ++ " takes an integer on each side")
            | otherwise                                             -> error ("unknown operator" ++ show op)
            where
                leftT       = getAndCheckExpr varList left
                rightT      = getAndCheckExpr varList right
                isBoolOp    = op `elem` ["and", "or"] 
                isIntOp     = op `elem` ["plus", "minus", "times", "DividedBy"]
                isIntBoolOp = op `elem` ["GreaterThan", "GreaterThanEq", "SmallerThan", "SmallerThanEq"]
        t                                                           -> error ("error at getAndCheckExpr --> " ++ show t)

-- get type of variable from varList
getType :: [(String,String)] -> AST -> String
getType [] (ASTNode Idf [ASTLeaf var])              = error ("Variable " ++ var ++ " not in scope")
getType varList (ASTNode Idf [ASTLeaf var, i])      = getType varList (ASTNode Idf [ASTLeaf var])
getType ((n,t):xs) t2@(ASTNode Idf [ASTLeaf var]) 
        | n == var                                  = t
        | otherwise                                 = getType xs t2
getType _ (ASTNode Value [ASTNode Integer _])       = "TypeInt"
getType _ (ASTNode Value [ASTNode Boolean _])       = "TypeBool"
getType [] t2@(ASTNode FuncCall (ASTNode FuncName [ASTLeaf name]:args)) = error ("Function " ++ name ++ " not in scope")
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
makeTupleTaskH (ASTNode Arg [ASTNode Type [ASTLeaf t], _]:ts) (n,tp) = makeTupleTaskH ts (n,tp ++ "," ++ t)
makeTupleTaskH (ASTNode Type [ASTLeaf t]:ts) (n,tp) = makeTupleTaskH ts (n,t ++ tp)
makeTupleTaskH (ASTNode Body kids:ts) (n,tp) 
        | ok = makeTupleTaskH ts (n,tp)
        | otherwise = error ("function " ++ n ++ " should return a " ++ expectedR ++ ", but returns a " ++ actualR)
        where
            actualR = getReturnType kids
            expectedR = TXT.unpack ((TXT.splitOn (TXT.pack ",") (TXT.pack tp))!!0)
            ok = actualR == expectedR
makeTupleTaskH (t:ts) tuple = error ("Node not supported in makeTupleTaskH --> " ++ show t)

getReturnType :: [AST] -> String -- kids of the body of the task should go in here only
getReturnType ts | getNodeType (head (reverse ts)) /= Idf = "TypeNothing"
getReturnType (ASTNode Decl [ASTNode Type [ASTLeaf t], idf]:ts)
        | idf == head (reverse ts) = t 
        | otherwise = getReturnType ts
getReturnType (ASTNode Decl [ASTNode Type [ASTLeaf t], idf, _]:ts)
        | idf == head (reverse ts) = t 
        | otherwise = getReturnType ts
getReturnType (t:ts) = getReturnType ts -- not a decl

makeTupleDecl :: [(String,String)] -> AST -> (String, String)
makeTupleDecl varList (ASTNode _ [_, ASTNode Idf [ASTLeaf nameStr]]) | inSameScope varList nameStr = error ("variable " ++ nameStr ++ " is already defined in this scope")
makeTupleDecl _ (ASTNode _ [ASTNode Type [ASTLeaf typeStr], ASTNode Idf [ASTLeaf nameStr]]) = (nameStr, typeStr)
makeTupleDecl _ (ASTNode Decl [ASTNode Type [ASTNode TypeArray [ASTNode Type [ASTLeaf typestr]]], ASTNode Idf [ASTLeaf nameStr]]) = (nameStr, (show TypeArray) ++ typestr)
makeTupleDecl _ t = error ("Node not supported in makeTupleDecl --> " ++ show t)

getNodeType :: AST -> Alphabet
getNodeType (ASTNode x _) = x

-- ==================================================
-- ==================================================
-- Clearly, you have to define your own embedded language for constrcuctions in your programming language.
--      This will naturally be a recursive algebraic type, such that it effectively represents the AST.
-- Besides, you'll have to transform a parsetree to a tree of this AST type.
-- Finally, the bck-end of the compiler (code-generation) will be a function of an AST into a list of SprIL instructions.
--
-- Just a hint: use pattern matching on trees

-- ========================================================
-- tokenizer
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
fixdots txt = g
            where
                a = TXT.replace (TXT.pack ",") (TXT.pack " , ") txt
                b = TXT.replace (TXT.pack ".") (TXT.pack " . ") a
                c = TXT.replace (TXT.pack ":") (TXT.pack " : ") b
                d = TXT.replace (TXT.pack "(") (TXT.pack " ( ") c
                e = TXT.replace (TXT.pack ")") (TXT.pack " ) ") d
                f = TXT.replace (TXT.pack "[") (TXT.pack " [ ") e
                g = TXT.replace (TXT.pack "]") (TXT.pack " ] ") f

removeComments :: [String] -> Bool -> [String]
removeComments [] b = []
removeComments (x:xs) True  | x == "btw" = removeComments xs False
                            | otherwise = x : removeComments xs True
removeComments (x:xs) False | x == "." = removeComments xs True
                            | otherwise = removeComments xs False