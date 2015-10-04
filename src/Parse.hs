module Parse where

data Parser a = P {runParser :: String -> [(a, String)]}

first f (a, b) = (f a , b)
instance Functor Parser where
  fmap f p = P $ map (first f) . runParser p
instance Applicative Parser where
  pure = return
  f <*> x = do
    f <- f
    x <- x
    return $ f x
instance Monad Parser where
  return x = P $ \s -> [(x, s)]
  x >>= f = P $ \s ->
    let ms = runParser x s
        step (a, str) = runParser (f a) str
    in concatMap step ms


char :: Char -> Parser Char
char x = P char'
  where
    char' (c : rest) | c == x = [(c, rest)]
    char' _ = []

string :: String -> Parser String
string str = mapM char str

parseAny :: [Parser a] -> Parser a
parseAny ps = P $ \s ->
  concatMap (\f -> runParser f s) ps

(<|>) a b = parseAny [a, b]

many, many1 :: Parser a -> Parser [a]
many p = parseAny [(:) <$> p <*> many p, return []]
many1 p = (:) <$> p <*> many p

many_ x = many x *> return ()

charSet = parseAny . map char

ws :: Parser Char
ws  = charSet $ " \n\t"
whitespace = many_ ws
newline = charSet "\n"

lowerAlpha = charSet $ ['a'..'z']
upperAlpha = charSet $ ['A'..'Z']
digit = charSet ['0'..'9']

idSym = charSet "-'"

idChar = lowerAlpha <|> upperAlpha <|> digit <|> idSym

identifier = do
  c <- lowerAlpha
  cs <- many (idChar)
  return $ c : cs

token x = (x <* many ws)

indent = many1 ws

sep :: Parser a -> Parser b -> Parser [b]
sep a b = ((:) <$> b <*> (a *> sep a b)) <|> (return <$> b)
sepBy = sep

data LHS = LHS [Pattern]
  deriving (Show, Eq, Ord)
type RHS = String
type Symbol = String
data Expr
  = App Symbol [Symbol]
  | Ref Symbol
  deriving (Show, Eq, Ord)
data Pattern
  = PExpr Expr
  | PBind Expr Symbol
  deriving (Show, Eq, Ord)
data Arrow = FnArrow | MutArrow
  deriving (Eq, Ord)
instance Show Arrow where
  show FnArrow = "->"
  show MutArrow = "~>"
data Rule = Rule LHS Arrow RHS
  deriving (Show, Eq, Ord)
data Def = Def String [Rule]
  deriving (Eq, Ord)

instance Show Def where
  show (Def name rules) =
    unlines $ name : map (("  " ++) . show) rules

finish :: Parser a -> Parser a
finish p = p <* (P finish')
  where
    finish' "" = [((), "")]
    finish' _ = []


symbol = token identifier

-- TODO there is some repetition in the output parse list, ie chkk pattern "a b"
expr :: Parser Expr
expr = argParser <|> appParser
  where
    argParser = Ref <$> identifier
    appParser = App <$> identifier <*> (many1 ws *> args)

pattern :: Parser Pattern
pattern = exprParser <|> bindParser
  where
    exprParser = PExpr <$> expr
    bindParser = bindR <|> bindL
    bindR = PBind <$> token expr <*> (token (char ')') *> identifier)
    bindL = flip PBind <$> token identifier <*> (token (char '(') *> expr)

lhs = LHS <$> sep (token $ char ',') (token pattern)
rhs = identifier
ruleArrow = (string "->" >> return FnArrow) <|> (string "~>" >> return MutArrow)

args = sepBy (many1 ws) identifier

nlws = newline >> whitespace
def  = Def  <$> identifier <*> (nlws *> sepBy nlws rule)
rule = Rule <$> lhs  <*> token ruleArrow <*> token rhs

prog = finish $ whitespace *> def


chk' p = head . runParser p
chk p = head . runParser (finish p)
chn n p = take n . runParser (finish p)
