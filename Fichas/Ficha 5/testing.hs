module Ficha5 where

import Data.Char (isDigit)
import Cp

import Data.Bool (bool)

f :: Num a => (a, a) -> a
f (a, b) = a +  b

f2 :: Num a => a -> a -> a
f2 a b = a + b

ap_1 :: (a -> b, a) -> b
ap_1 = uncurry ($)

ap_2 :: (a -> b, a) -> b
ap_2 = uncurry (\f x -> f x)

ap_3 :: (a -> b, a) -> b
ap_3 = uncurry id

ap_4 :: (a -> b, a) -> b
ap_4 (f, a) = ($) f a

ap_5 :: (a -> b, a) -> b
ap_5 (f, a) = f a

testFunction = ap_4 . (curry f >< id)

testFunction2 = ap_1 . (f2 >< id)

-- ap . (curry f >< id) = f
-- curry f a b = f (a, b)

-- uncurry :: (x       -> y -> z) -> (x -> y) -> z
-- ($)     :: (a -> b) -> a -> b

-- ------------------------------------------------

data Point a = Point { x :: a, y :: a, z :: a } deriving (Eq, Show)
-- :t Point
-- Point :: a -> a -> a -> Point a

out :: Point a -> ((a, a), a)
out (Point x y z) = ((x, y), z)

out2 :: Point a -> ((a, a), a)
out2 = split (split x y) z

in' :: ((a, a), a) -> Point a
in' ((x, y), z)= Point x y z

in'' :: ((a, a), a) -> Point a
in'' = ap . (uncurry Point >< id)

in''' :: ((a, a), a) -> Point a
in''' = uncurry $ uncurry Point

-- Point :: a -> a -> a -> Poiny a
-- uncurry Point :: (a, a) -> a -> Point a
-- uncurry (uncurry Point) :: ((a, a), a) -> Point a

-- uncurry :: (a->b->c) -> (a,b) -> c
-- uncurry f (x,y) = f x y

-- => uncurry (uncurry Point ((a,a), a))
-- = uncurry (Point (a, a) a)
-- = Point a a a

-- função aleatoria, n faz parte da ficha
-- strToList = map (read . filter isDigit :: String -> Int) . words
-- strToList = read :: String -> [Int]

alpha :: Either a a -> (Bool, a)
alpha = either (split false id) (split true id)

aaalpha :: Either a b -> (Bool, Either a b)
aaalpha = either (split false i1) (split true i2)

singl' :: x -> [x]
singl' x = [x]

delta :: Either [(a, b)] (a, b) -> Either [b] [a]
delta = either (i1 . map p2) (i2 . singl . p1)

delta' :: Either [(a, a)] (a, a) -> [a]
delta' = either (map p2) (singl . p1)

test :: (a, Bool) -> (a, Either () ())
test (x, True) = (x, i1 ())
test (x, False) = (x, i2())

test2 = curry $ ap . (id >< reverse)
test3 f = f . reverse

-- o curry permite que uma função que recebe os argumentos em tuplos
-- receba um unico argumento e retorna outra função que aceita os 
-- restantes argumentos, um por um
-- f :: a -> b -> c     <=> f :: a -> (b -> c)