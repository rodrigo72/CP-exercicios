module Ficha3 where

import Cp 

store c = take 10 . nub' . (c:)

-- removes duplicates
myNub :: (Eq a) => [a] -> [a]
myNub [] = []
myNub (x:xs) = x : myNub (filter (/= x) xs)

nub' :: (Eq a) => [a] -> [a]
nub' = either nil cons . f

f :: (Eq a) => [a] -> Either [a] (a, [a])
f [] = Left []
f l@(x:xs) = Right (x, myNub (filter (/= x) xs))