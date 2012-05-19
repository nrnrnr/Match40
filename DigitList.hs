module DigitList
       ( fromInterval, evalWithBase, ratDigits
       , d10
       , inInterval, minimalLength, inRange
       )
where
  
import Debug.Trace
import Data.Ratio

import Test.QuickCheck

fromInterval :: Integral a => a -> Ratio a -> Ratio a -> [a]
fromInterval base lo hi =
    if r 0 <= lo && lo < hi && hi <= r 1 then
      if r dplus < r base * hi then
        if dplus == 0 then [] else [dplus]
      else d0 : fromInterval base (expand lo) (expand hi)
    else
      error $ "interval [" ++ show lo ++ ", " ++ show hi ++
              ") outside the unit interval"
  where dplus = myceil (r base * lo)
        d0    = dplus - 1
        expand x = r base * x - r d0
        r = fromIntegral
        myceil r = (n + d - 1) `div` d
          where (n, d) = (numerator r, denominator r)

evalWithBase :: Integral a => a -> [a] -> Ratio a
evalWithBase base [] = 0
evalWithBase base (d:ds) = (fromIntegral d + evalWithBase base ds) / fromIntegral base

ratDigits base r = fromInterval base r (nextrat r)

nextrat r = (numerator r + 1) % denominator r

inInterval base (NonNegative num) (Positive den') = ok
  where x' = evalWithBase base (fromInterval base (num % den) (inc num % den))
        x = num % den
        x1 = inc num % den
        ok = x <= x' && x' < x1
        inc n = n + 1
        den = den' `max` num + 1

inRange (Positive base') (NonNegative num) (Positive den') = ok
  where ok = all inrange (fromInterval base (num % den) (inc num % den))
        inrange d = 0 <= d && d < base
        inc n = n + 1
        den = den' `max` (num + 1)
        base = base' + 1

minimalLength base (NonNegative num) (Positive den) = not (null ds) ==> ok
  where x' = evalWithBase base (init ds)
        ds = fromInterval base (num % den) (inc num % den)
        x = num % den
        x1 = inc num % den
        ok = not (x <= x' && x' < x1)
        inc n = n + 1


d10 n d = fromInterval 10 (n % d) ((n + 1) % d)
