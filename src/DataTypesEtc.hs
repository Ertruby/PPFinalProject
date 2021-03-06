{-# LANGUAGE FlexibleInstances #-}
module DataTypesEtc where

-- all datatypes are defined here so we can import this file when we need one of the types.
-- because we do it like this we can make the project more modular because otherwise modules have to import each other.

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
                | Line
                | Expr
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
                | NotEqual
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
                
type Grammar = Alphabet -> [[Alphabet]]

type Token      = (Alphabet,String)

data ParseTree  = PLeaf Token                   -- PLeaf: ParseTree-Leaf
                | PNode Alphabet [ParseTree]    -- PNode: ParseTree-Node
                deriving (Eq,Show)

type ParseState = ( Alphabet                    -- Non-terminal indicating the present subexpression
                  , [ParseTree]                 -- The already produced trees within the present subexpression
                  , [Token]                     -- The remaining list of input tokens
                  )
                  
data AST = ASTNode Alphabet [AST] | ASTLeaf String deriving (Show, Eq)