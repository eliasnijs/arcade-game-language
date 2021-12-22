module Parser where

import Control.Applicative
import Keywords

newtype Parser a =
  Parser
    { parse :: String -> Maybe (String, a)
    }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \t -> do
      (t', x) <- p t
      Just (t', f x)

instance Applicative Parser where
  pure x = Parser $ \t -> Just (t, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \t -> do
      (t', f) <- p1 t
      (t'', a) <- p2 t'
      Just (t'', f a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \t -> p1 t <|> p2 t

pChar :: Char -> Parser Char
pChar x = Parser f
  where
    f (y:ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

pString :: String -> Parser String
pString = traverse pChar

pSpan :: (Char -> Bool) -> Parser String
pSpan f =
  Parser $ \t ->
    let (c, n) = span f t
     in Just (n, c)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser $ \t -> do
    (t', xs) <- p t
    if null xs
      then Nothing
      else Just (t', xs)

endWidth, seperateBy :: Parser a -> Parser b -> Parser [b]
endWidth s e = many (e <* s) <|> pure []

seperateBy s e = (:) <$> e <*> many (s *> e) <|> pure []

blank, sBlank, nBlank, notBlank :: Parser String
blank = pSpan (\x -> x == ' ' || x == '\n')

sBlank = pSpan (== ' ')

nBlank = pSpan (== '\n')

notBlank = pSpan (\x -> x /= ' ' && x /= '\n')
