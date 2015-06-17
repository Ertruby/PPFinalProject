module Tokenizer where
import Prelude
import Data.Char
import Data.List
import qualified Data.Text as TXT

-- error ("Dear sir,\n We have noticed that you have made a mistake in your beloved code. You have used a word that we do not recognize, namely" ++ x ++ ". We kindly ask you to reconsider your code.\n Thanks in advance, we hope we can help you further next time.\n Good luck!")

data T = Error | Suppose | Intt | Boolt | Chart | Intval | Truet | Falset | Charval | Task | Fname | Takes | Comma | And | Gives | Btw | Dot | To | While | Is | Do | Incr | Plus | Minus | Times | Devides | When | Otherwise | Give | Nothingt | After | Varname | Greater | Or | Than | Smaller | Equals | Equal deriving (Show, Eq)

keys = ["suppose", "integer", "boolean", "character", "task", "takes", ",", "and", "gives", "btw,", ".", "to", "while", "is", "do:", "increment", "plus", "minus", "times", "devides", "when", "otherwise", "nothing", "give", "after:", "greater", "or", "than", "smaller", "equals", "equal"]
keyTokens = [Suppose, Intt, Boolt, Chart, Task, Takes, Comma, And, Gives, Btw, Dot, To, While, Is, Do, Incr, Plus, Minus, Times, Devides, When, Otherwise, Nothingt, Give, After, Greater, Or, Than, Smaller, Equals, Equal]

tok :: String -> [(T,String)]
tok str = tokH (prepare str)

tokH :: [String] -> [(T,String)]
tokH [] = []
tokH (x:xs) | isUpper (head x) = (Fname, x) : tokH xs
            | x == "true" = (Truet, x)  : tokH xs
            | x == "false" = (Falset, x) : tokH xs
            | not (False `elem` (map isDigit x)) = (Intval, x) : tokH xs
            | head x == '\'' && head (reverse x) == '\'' && length x == 3 = (Charval, x) : tokH xs
            | a /= Error = (a,b) : tokH xs
            | otherwise = (Varname, x) : tokH xs
                where
                    (a,b) = getToken x
       
getToken :: String -> (T,String)       
getToken str = getTokenH keys keyTokens str
getTokenH :: [String] -> [T] -> String -> (T,String)
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
fixdots txt = TXT.replace (TXT.pack ".") (TXT.pack " .") txt

removeComments :: [String] -> Bool -> [String]
removeComments [] b = []
removeComments (x:xs) True | x == "btw," = removeComments xs False
                            | otherwise = x : removeComments xs True
removeComments (x:xs) False | x == "." = removeComments xs True
                            | otherwise = removeComments xs False