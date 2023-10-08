module Ex7 where

import Data.List

type Key = Int
type Aut = String
type Pag = Int

type Bib = [(Key, [Aut])]
type Aux = [(Pag, [Key])]
type Ind = [(Aut, [Pag])]

p1 :: (a,b) -> a
p1 (a,b) = a

p2 :: (a,b) -> b
p2 (a,b) = b

getAut :: Bib -> [Aut]
getAut a = (sort . nub) $ foldl (\acc x -> snd x ++ acc) [] a

getCena :: (Eq a, Eq b) => a -> [(b,[a])] -> [b]
getCena a b = foldl (\acc x -> p1 x : acc) [] (filter (\x -> a `elem` p2 x) b)

getInd :: (Bib,Aux) -> Ind
getInd (a,b) = foldl (\acc x -> acc ++ [(x, (nub . sort) $ foldl(\acc1 x1 -> (getCena x1 b) ++ acc1) [] (getCena x a))]) [] (getAut a)

main :: IO ()
main = do
    let bib1 = [ (1, ["asd", "dfg"]), (2, ["lkj", "ppp"]), (2, ["asd", "llll"]) ]
        aux1 = [ (1, [1, 2]), (2, [2]) ]
        ind1 = getInd (bib1, aux1)
    print ind1