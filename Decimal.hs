module Decimal
       ( digits, number
       , d10
       , inInterval
       )
where
  
import Debug.Trace
import Data.Ratio

import Test.QuickCheck

digits :: Integral a => a -> Ratio a -> Ratio a -> [a]
digits base lo hi =
    if r dplus < r base * hi then
      if dplus == 0 then [] else [dplus]
    else d0 : digits base (expand lo) (expand hi)
  where dplus = myceil (r base * lo)
        d0    = dplus - 1
        expand x = r base * x - r d0
        r = fromIntegral
        myceil r = (n + d - 1) `div` d
          where (n, d) = (numerator r, denominator r)

number :: Integral a => a -> [a] -> Ratio a
number base [] = 0
number base (d:ds) = (fromIntegral d + number base ds) / fromIntegral base


inInterval base (NonNegative num) (Positive den) = ok
  where x' = number base (digits base (num % den) (inc num % den))
        x = num % den
        x1 = inc num % den
        ok = x <= x' && x' < x1
        inc n = n + 1

minimalLength base (NonNegative num) (Positive den) = not (null ds) ==> ok
  where x' = number base (init ds)
        ds = digits base (num % den) (inc num % den)
        x = num % den
        x1 = inc num % den
        ok = not (x <= x' && x' < x1)
        inc n = n + 1


d10 n d = digits 10 (n % d) ((n + 1) % d)
