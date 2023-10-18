module Ficha4 where

import Cp

-- key-value NoSQL data model

glue :: ([(k1, v)], [(k2, v)]) -> [(Either k1 k2, v)] 
glue ([], []) = []
glue (a, b) = map (\(k, v) -> (Left k, v)) a ++ map (\(k, v) -> (Right k, v)) b

glue2 :: ([(a, v)], [(b, v)]) -> [(Either a b, v)]
glue2 (a, b)= (map (i1 >< id) a) ++ (map (i2 >< id) b)

glue3 :: ([(a, v)], [(b, v)]) -> [(Either a b, v)]
glue3 = uncurry (++) . (map (i1 >< id) >< map (i2 >< id))

-- rip isomorfismo
-- glueNub :: (Eq a, Eq b) => [(a,b)] -> [(a, b)]
-- glueNub [] = []
-- glueNub ((a,b):xs) = (a,b) : glueNub (filter (\(c,d) -> d /= b) xs)

insereLeft :: a -> ([a], [b]) -> ([a], [b])
insereLeft x (left, right) = (x : left, right)

insereRight :: b -> ([a], [b]) -> ([a], [b])
insereRight x (left, right) = (left, x : right)

unglue :: [(Either k1 k2, v)] -> ([(k1, v)], [(k2, v)])
unglue [] = ([],[])
unglue (x:xs) = case distl x of 
                    Left n -> insereLeft n (unglue xs)
                    Right n -> insereRight n (unglue xs)

type Key_1 = Int
type Key_2 = String
type Value = Int 

testUnglue :: [(Either Key_1 Key_2, Value)]
testUnglue = [(Left 1, 3), (Left 2, 2), (Right "key", 2)]

testGlue :: ([(Key_1, Value)], [(Key_2, Value)])
testGlue = ([(1, 2), (2, 3), (3, 4)], [("asd", 1), ("aa", 3), ("w", 5)])