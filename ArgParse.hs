module ArgParse
       ( Parser, parse
       , oneArg, pread, pmaybe, (<|>), pfail, pstring, eoargs
       )
       where

import Control.Applicative

newtype Parser a = P ([String] -> [(a, [String])])

parse (P p) = p

instance Functor Parser where
  fmap f (P p) = P $ \ss -> [(f a, ss') | (a, ss') <- p ss]

instance Applicative Parser where
  pure a = P $ \ss -> [(a, ss)]
  P pf <*> P pa = P $ \ss -> [(f a, ss') | (f, ss'') <- pf ss, (a, ss') <- pa ss'' ]
  
instance Alternative Parser where
  P p <|> P q = P $ \ss -> p ss ++ q ss
  empty = P $ \ _ -> []

oneArg :: (String -> [a]) -> Parser a
oneArg convert = P parse
  where parse [] = []
        parse (s:ss) = [(a, ss) | a <- convert s]

pread :: Read a => Parser a
pread = oneArg convert
  where convert s = [ a | (a, t) <- reads s, ("", "") <- lex t ]
        
pmaybe :: Parser a -> Parser (Maybe a)
pmaybe p = fmap Just p <|> pure Nothing

pfail :: Parser a
pfail = empty

pstring :: Parser String
pstring = oneArg $ \s -> [s]

eoargs :: Parser ()
eoargs = P $ \ss -> if null ss then [((), [])] else []
