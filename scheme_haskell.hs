module Main where

import Control.Monad
import System.Environment
-- Adding Parsec library function available to us, except the spaces function, whose name conflicts with a funciton that we;ll be defiing later.
import Text.ParserCombinators.Parsec hiding (spaces)
--IO is an action
main :: IO ()
main = getArgs >>= putStrLn . show . eval . readExpr . (!! 0)

-- example of monad, "extra information" is all the info about position in input stream, back tracking record, first and follow sets, etc. 
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"


--readExpr is a function (->) from a string to a ThrowsError LispVal, 
--readExpr: String->String

--we name the parameter input, pass it, along with symbol action we defined above and the name of the parser ("lisp") to Parsec functino parse. 

--parse can return either parse value or errror. Parser returns an either data type, using Left constructor to indicate an error and right one for a normal value 

--case: of construction is an exmaple of pattern matching.


readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

spaces :: Parser ()
spaces = skipMany1 space


{-This defines a data type, set of possible values that a variable of type LispVal can hold. Each Alternative (constructor replaced by |) contains a Tag.

Atom: stores a String naming atom
List: stores a list of LispVal
DottedList: represents scehem of form (a b . c); (Improper List) stores all elements except the last and last element on another field.
Number: Haskell Integer
String: Haskell String
Bool: Haskell Boolean value
-}

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
{-
we are adding more parsing functions to create value of these types
-}
parseString :: Parser LispVal
{-

Using do-notation for binding. Retrieving vale of our parse (returned by many (noneOf "\"")) and manipulating it. 

Parsing String here by function composition.
-}
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x
{-
Parsing Atom using Parsec combinator, using choice operator <|>, This tries the first parser, if it fails
  it tries the second. If either suceeds then it returns the value returned by that parser.

  First parser must fail if it consumes the input and implements backtracking.
-}

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of 
                          "#t" -> Bool True
                          "#f" -> Bool False
                          otherwise -> Atom atom
{-ANother parser, for numbers and so on other parsers are implemented-}
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit
{-
Another parse to parse List using inbuilt functions in Parser.
-}
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- (try parseList) <|> parseDottedList
               char ')'
               return x
{-
it reads a single quote characters, reads an expression and binds to x and then return (quote x) to use scheme notation.

Atom constructor, pass the string we encapsulated and gives LispVal. We can manipulated LispVal now
-}
showVal :: LispVal -> String

{-
we are doing pattern matching here, selecting a code and then binding it's name with constructor. Any constructor can appear
if tag is the same name. 


-}

showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"

{-
The List and DottedList clauses work similarly, but to define a
helper function unwordsList to convert the contained list into a string:
-}

showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

{-Evaluator: Mapping code data type to other code data type and result of evaluation.

Evaluating: String, booleans and Quote: simply return the value.


The built in function looks as key in a list of pairs and return a bool if found.

-}
primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

{- 


The functions that we store are themselves the result of a function,
numericBinop, This takes a primitive Haskell
function (often an operator section) and wraps it with code to unpack an argument
list, apply the function to it, and wrap the result up in our Number
constructor.

-}
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n in 
                          if null parsed 
                            then 0
                            else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0