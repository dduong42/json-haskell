import Data.Char (isControl)


newtype Parser a = Parser (String -> [(a, String)])
type Object = [(String, Value)]
data Value = StrValue String | ObjValue Object | BoolValue Bool | ArrValue [Value]
             | Null


instance Monad Parser where
    (>>=) (Parser p) f = Parser g
        where g s = case p s of [] -> []
                                [(x, y)] -> h y where Parser h = f x
    return x = Parser (\s -> [(x, s)])


(<|>) :: Parser a -> Parser a -> Parser a
Parser f <|> Parser g = Parser h
    where h s = case f s of [] -> g s
                            _ -> f s


item :: Parser Char
item = Parser (\x -> case x of "" -> []
                               (x:xs) -> [(x, xs)])


filterParser :: (a -> Bool) -> Parser a -> Parser a
filterParser f (Parser p) = Parser (\s -> [(x, s') | (x, s') <- p s, f x])


char :: Char -> Parser Char
char c = filterParser (\x -> x == c) item


normalUTFChar :: Parser Char
normalUTFChar = filterParser filterFunc item
    where filterFunc c = c /= '\\' && c /= '"' && not (isControl c)


hexDigit' :: String -> [(Int, String)]
hexDigit' ('0':xs) = [(0, xs)]
hexDigit' ('1':xs) = [(1, xs)]
hexDigit' ('2':xs) = [(2, xs)]
hexDigit' ('3':xs) = [(3, xs)]
hexDigit' ('4':xs) = [(4, xs)]
hexDigit' ('5':xs) = [(5, xs)]
hexDigit' ('6':xs) = [(6, xs)]
hexDigit' ('7':xs) = [(7, xs)]
hexDigit' ('8':xs) = [(8, xs)]
hexDigit' ('9':xs) = [(9, xs)]
hexDigit' ('a':xs) = [(10, xs)]
hexDigit' ('A':xs) = [(10, xs)]
hexDigit' ('b':xs) = [(11, xs)]
hexDigit' ('B':xs) = [(11, xs)]
hexDigit' ('c':xs) = [(12, xs)]
hexDigit' ('C':xs) = [(12, xs)]
hexDigit' ('d':xs) = [(13, xs)]
hexDigit' ('D':xs) = [(13, xs)]
hexDigit' ('e':xs) = [(14, xs)]
hexDigit' ('E':xs) = [(14, xs)]
hexDigit' ('f':xs) = [(15, xs)]
hexDigit' ('F':xs) = [(15, xs)]
hexDigit' _ = []


hexDigit :: Parser Int
hexDigit = Parser hexDigit'


fourHexDigit :: Parser Int
fourHexDigit = do { x3 <- hexDigit
                  ; x2 <- hexDigit
                  ; x1 <- hexDigit
                  ; x0 <- hexDigit
                  ; return (x0 + x1 * 16 + x2 * 256 + x3 * 4096)
                  }


jsonChar :: Parser Char
jsonChar =  normalUTFChar
            <|> do {char '\\'; char '"'; return '"'}
            <|> do {char '\\'; char '\\'; return '\\'}
            <|> do {char '\\'; char 'b'; return '\b'}
            <|> do {char '\\'; char 'f'; return '\f'}
            <|> do {char '\\'; char 'n'; return '\n'}
            <|> do {char '\\'; char 'r'; return '\r'}
            <|> do {char '\\'; char 't'; return '\t'}
            <|> do {char '\\'; char 'u'; n <- fourHexDigit; return (toEnum n)}


many :: Parser a -> Parser [a]
many p = many1 p <|> return []


many1 :: Parser a -> Parser [a]
many1 p = do { c <- p
             ; cs <- many p
             ; return (c:cs)
             }


jsonString :: Parser String
jsonString = do { char '"'
                ; x <- many jsonChar
                ; char '"'
                ; return x
                }
