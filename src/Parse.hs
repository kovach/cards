-- TODO this parser is wonky
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
ws' = charSet $ " \t"
whitespace = many_ ws
whitespace' = many_ ws'
newline = charSet "\n"

lowerAlpha = charSet $ ['a'..'z']
upperAlpha = charSet $ ['A'..'Z']
digit = charSet ['0'..'9']

idSym = charSet "-'"

idChar = lowerAlpha <|> upperAlpha <|> digit <|> idSym

identifier' first = (:) <$> first <*> many idChar
identifier = identifier' lowerAlpha
predIdentifier = identifier' upperAlpha

token x = (x <* many ws)

indent = many1 ws

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy a b = ((:) <$> b <*> (a *> sepBy a b)) <|> (return <$> b)

type Symbol = String
data Value
  = Num Int
  | Ref Symbol
  | Member Symbol Symbol
  deriving (Show, Eq, Ord)
data Expr
  = App Symbol [Value]
  | EVal Value
  deriving (Show, Eq, Ord)
data Binder = Binder Expr Symbol
  deriving (Show, Eq, Ord)
data Predicate = Predicate Symbol [Value]
  deriving (Show, Eq, Ord)
data Pattern
  = PExpr Expr
  | PBind Binder
  | PPred Predicate
  deriving (Show, Eq, Ord)
data Action
  = Mutate Symbol Symbol Expr
  | AExpr Expr
  | ABind Binder
  | APred Predicate
  deriving (Show, Eq, Ord)
data LHS = LHS [Pattern]
  deriving (Show, Eq, Ord)
data ERHS = ERHS Expr
  deriving (Show, Eq, Ord)
data MRHS = MRHS [Action]
  deriving (Show, Eq, Ord)
data Arrow = FnArrow | MutArrow
  deriving (Eq, Ord)
instance Show Arrow where
  show FnArrow = "->"
  show MutArrow = "~>"
data Rule = ERule LHS ERHS | MRule LHS MRHS
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

readInt :: String -> [(Int, String)]
readInt = reads

value :: Parser Value
value = num <|> ref <|> member
  where
    num = Num <$> P readInt
    ref = Ref <$> identifier
    member = Member <$> identifier <*> (char '.' *> identifier)

spaces = sepBy (many1 ws)

args = spaces value

expr :: Parser Expr
expr = (EVal <$> value) <|> appParser
  where
    appParser = App <$> identifier <*> (many1 ws *> args)

binder :: Parser Binder
binder = bindR <|> bindL
  where
    bindR = Binder <$> token expr <*> (token (char ')') *> identifier)
    bindL = flip Binder <$> token identifier <*> (token (char '(') *> expr)

predicate :: Parser Predicate
predicate = Predicate <$> predIdentifier <*> (many1 ws *> args)

pattern :: Parser Pattern
pattern = (PExpr <$> expr) <|> (PBind <$> binder) <|> (PPred <$> predicate)

-- TODO
action :: Parser Action
action = (ABind <$> binder) <|> (AExpr <$> expr) <|> (APred <$> predicate)
         <|> mutate
  where
    mutate = Mutate <$> identifier <*> (char '.' *> token identifier)
                    <*> (token (string "<-") *> expr)

commas x = sepBy (whitespace >> token (char ',')) x

lhs = LHS <$> commas (pattern)
erhs = ERHS <$> expr
mrhs = MRHS <$> commas (action)

earrow = (string "->" >> return FnArrow)
marrow = (string "~>" >> return MutArrow)

nlws = whitespace' >> many1 newline >> whitespace'
wslr x = whitespace *> x <* whitespace
endDef = string "\n.\n"
def  = Def  <$> identifier <*> (nlws *> sepBy nlws rule <* endDef)
rule = erule <|> mrule
  where
    erule = ERule <$> token lhs <*> (token earrow *> erhs)
    mrule = MRule <$> token lhs <*> (token marrow *> mrhs)

prog :: Parser [Def]
prog = wslr $ many def


chk' p = head . runParser p
chk p = runParser (finish p)
chn n p = take n . runParser (finish p)
