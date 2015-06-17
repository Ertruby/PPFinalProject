module Tokenizer where
import Prelude
import Data.Char
import Data.List
import qualified Data.Text

data T = Suppose | Intt | Boolt | Chart | Intval | Boolval | Charval | Task | Fname | Takes | Comma | And | Gives | Btw | Dot | To | While | Is | Do | Incr | Plus | Minus | Times | Devides | When | Otherwise | Give | Nothing | After

keys = ["suppose", "integer", "boolean", "character", "task", "takes", ",", "and", "gives", "btw,", ".", "to", "while", "is", "do:", "increment", "plus", "minus", "times", "devides", "when", "otherwise", "nothing", "give", "after:"]
keyTokens = [Suppose, Intt, Boolt, Chart, Task, Takes, Comma, And, Gives, Btw, Dot, To, While, Is, Do, Incr, Plus, Minus, Times, Devides, When, Otherwise, Nothing, Give, After]
tok :: String -> [(T,String)]
tok str = tokH (map Data.Text.unpack (Data.Text.splitOn (Data.Text.pack " ") (Data.Text.pack str)))

tokH :: [String] -> [(T,String)]
tokH [] = []
tokH (x:xs) = tuple : tokH xs
                where
                    tuple = getToken x
       
getToken :: String -> (T,String)       
getToken str = getTokenH keys keyTokens str
getTokenH :: [String] -> [T] -> String
getTokenH [] [] s = (Nothing, "nothing")
getTokenH (x:xs) (y:ys) s | s == x = (y,x)
                          | otherwise = getTokenH xs ys s