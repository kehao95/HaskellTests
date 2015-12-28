module Solution where

import Data.Char

-- Parser type
newtype Parser a = AParser { func :: String -> Maybe (a, String) }

parse :: Parser a -> String -> Maybe (a, String)
parse p s = func p s

success :: a -> Parser a
success a = AParser $ \s -> Just (a, s)

failure :: Parser a
failure = AParser $ \_ -> Nothing

infixl 2 |||
(|||) :: Parser a -> Parser a -> Parser a
p ||| q = AParser $ \s -> case (parse p s) of 
  {
    Just (a, s')  -> Just (a, s');
    Nothing       -> parse q s
  }

instance Monad Parser where
  p >>= q = AParser $ \s -> case (parse p s) of
    {
      Just (a, s')  -> parse (q a) s';
      Nothing       -> Nothing
    }
  return a = success a

getonemore :: Parser a -> Parser [a]
getonemore p = do
  a <- p
  as <- getonemore p ||| success []
  return (a:as)
  
getchar :: Parser Char
getchar = AParser $ \s -> case s of 
  {
    (c:s')  -> Just (c, s');
    _       -> Nothing
  }

getcharif :: (Char -> Bool) -> Parser Char
getcharif p = do 
  c <- getchar
  if p c then return c else failure

getchars :: [Char] -> Parser [Char]
getchars (x:xs) = do
  getcharif (==x)
  getchars xs
  return (x:xs)
getchars [] = success []

getzeromore :: Parser a -> Parser [a]
getzeromore p = getonemore p ||| success []

-- Expression type
data Expression = NumExpr Integer | BinExpr String Expression Expression | UniExpr String Expression

instance Show Expression where
  show (NumExpr a) = show a 
  show (UniExpr op e) = op ++ "(" ++ (show e) ++ ")"
  show (BinExpr op e1 e2) = "("++ (show e1) ++ ")" ++ op ++ "(" ++ (show e2) ++ ")"

eval :: Expression -> Integer
eval (NumExpr a) = a
eval (BinExpr op e1 e2) = case op of 
  "+" -> (eval e1) + (eval e2)
  "*" -> (eval e1) * (eval e2) 
  "-" -> (eval e1) - (eval e2)
  "/" -> (eval e1) `div` (eval e2)  
eval (UniExpr op e) = case op of
  "-" -> - (eval e)

getnumexpr :: Parser Expression
getnumexpr = do
  s <- getonemore (getcharif isDigit)
  s' <- (do
      getcharif (=='.')
      s'' <- getzeromore (getcharif isDigit)
      return ('.':s'')
    ) ||| success []
  return (NumExpr (read (s ++ s') ::Integer))

getparenexpr :: Parser Expression
getparenexpr = do
  getcharif (=='(')
  e <- getsumexpr
  getcharif (==')')
  return e

getprimexpr :: Parser Expression
getprimexpr = getparenexpr ||| getnumexpr ||| getnegexpr ||| getfuncexpr

getuniexpr :: Parser Expression -> Parser [Char] -> Parser Expression
getuniexpr exprparser operatorparser = do
  op <- operatorparser
  e  <- exprparser
  return (UniExpr op e)

getnegexpr :: Parser Expression
getnegexpr = getuniexpr (getnumexpr ||| getparenexpr ||| getfuncexpr) (getchars "-")

getfuncexpr :: Parser Expression
getfuncexpr = getuniexpr getparenexpr (getchars "sqrt")

getbinexpr :: Parser Expression -> Parser [Char] -> Parser Expression
getbinexpr exprparser operatorparser = do
  e   <- exprparser
  es  <- getonemore ( do
      op <- operatorparser
      e' <- exprparser
      return (op, e')
    ) ||| success []
  return (foldl (\a -> \b -> (BinExpr (fst b) a (snd b))) e es)

getprodexpr :: Parser Expression
getprodexpr = getbinexpr getprimexpr (getchars "*" ||| getchars "/")

getsumexpr :: Parser Expression
getsumexpr = getbinexpr getprodexpr (getchars "+" ||| getchars "-")

parseeval :: Parser Expression -> String -> Maybe Integer
parseeval p s = do 
  e <- parse p s
  return (eval (fst e))

solution :: [Char] -> Maybe Integer
solution s = parseeval getsumexpr s

