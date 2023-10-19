module Ficha5 where

import Cp

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

