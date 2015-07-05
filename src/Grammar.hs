module Grammar where 
import DataTypesEtc

grammar :: Grammar

-- the potato grammar
grammar nt = case nt of

        Program -> [[prog, FuncName, ProgBody]]
        
        ProgBody    -> [[semi, Rep0 [Line], stop, dot]]
                
        Line    -> [[Decl]
                    ,[Assign]
                    ,[FuncCall]
                    ,[Incr]
                    ,[When]
                    ,[While]
                    ,[Task]]

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
                    ,[NotEqual]
                    ,[GreaterThan]
                    ,[GreaterThanEq]
                    ,[SmallerThan]
                    ,[SmallerThanEq]
                    ,[andK]
                    ,[orK]]
        
        NotEqual    -> [[is, notK, equal, to]]
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
notK         = Keyword "not"