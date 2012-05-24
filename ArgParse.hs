module ArgParse
       ( Parser, parse, uniqueParse
       , oneArg, mapFirstArg
       , pread, pmaybe, pfail, pstring, eoargs, pminus, pelse
       , oneUpper, oneLower
       )
       where

import Control.Applicative
import Data.Char

newtype Parser a = P ([String] -> [(a, [String])])

parse (P p) = p

uniqueParse :: Parser a -> [String] -> Maybe a
uniqueParse (P p) ss = case p ss of [(a, [])] -> Just a
                                    _ -> Nothing

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

-- | makes first word all upper case
oneUpper :: Parser a -> Parser a
-- | makes first word all lower case
oneLower :: Parser a -> Parser a

oneUpper = mapFirstArg (map toUpper)
oneLower = mapFirstArg (map toLower)

-- | @mapFirstArg f p@ is a parser that behaves like @p@,
-- except that the first argument seen has had @f@ 
-- applied to it
mapFirstArg :: (String -> String) -> Parser a -> Parser a
mapFirstArg f (P p) = P $ \ss -> case ss of [] -> p []
                                            (s:ss) -> p (f s : ss)

-- | Left biased alternative: if the left succeeds, the right cannot
pelse :: Parser a -> Parser a -> Parser a
P p `pelse` P q = P $ \ss -> if null (p ss) then q ss else p ss

pread :: Read a => Parser a
pread = oneArg convert
  where convert s = [ a | (a, t) <- reads s, ("", "") <- lex t ]
        
pmaybe :: Parser a -> Parser (Maybe a)
pmaybe p = fmap Just p <|> pure Nothing
--- XXX TODO pmaybe is 'optional'?

pfail :: Parser a
pfail = empty

pminus :: Parser a -> Parser b -> Parser a
pminus (P p) (P q) = P $ \ss -> if null (q ss) then p ss else []

pstring :: Parser String
pstring = oneArg $ \s -> [s]

eoargs :: Parser ()
eoargs = P $ \ss -> if null ss then [((), [])] else []
