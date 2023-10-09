module Questoes where

-- 08/10/2023

enumFromTo' :: Int -> Int -> [Int]
enumFromTo' start end
    | start > end = []
    | otherwise = start : enumFromTo' (start + 1) end

-- (:) adds an element to the front of a list. Example: 1 : [2,3,4] = [1,2,3,4]
-- enumFromTo' 1 3 => 1 : (2 : (3 : [])) => 1 : (2 : [3]) => 1 : [2,3] => [1,2,3]

myEnumFromThenTo :: Int -> Int -> Int -> [Int]
myEnumFromThenTo start next end
    | start > end && next >= start || start < end && next < start = []
    | otherwise = start : enumFromThenTo next (2 * next - start) end

-- enumFromThenTo 1 3 6 => [1,3,5]; enumFromThenTo 10 8 1 => [10,8,6,4,2]

myPlusPlus :: [a] -> [a] -> [a]
myPlusPlus [] l2 = l2
myPlusPlus l1 [] = l1 -- unnecessary  
myPlusPlus (x:xs) l2 = x : myPlusPlus xs l2

-- (++) [1,2,3] [4,5] => 1 : (2 : (3 : [4,5])) => [1,2,3,4,5]

myExclamationMarkExclamationMark :: [a] -> Int -> a 
myExclamationMarkExclamationMark (x:xs) pos 
    | pos == 0 = x
    | otherwise = myExclamationMarkExclamationMark xs (pos-1)

-- or
-- (!!) :: [a] -> Int -> a
-- (!!) (h:_) 0 = h
-- (!!) (_:t) n = (!!) t (n - 1)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

reverse'' :: [a] -> [a]
reverse'' l = reverseAcc l []

reverseAcc :: [a] -> [a] -> [a]
reverseAcc (x:xs) acc = reverseAcc xs (x:acc)
reverseAcc [] acc = acc

take' :: Int -> [a] -> [a]
take' _ [] = []
take' n (x:xs) 
    | n <= 0 = []
    | otherwise = x : take (n-1) xs

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 l = l
drop' n (x:xs)
    | n > 0 = drop' (n-1) xs

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = [] -- both cases are necessary
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x : replicate' (n-1) x

intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [x] = [x]
intersperse' x (y:ys) = y : x : intersperse' x ys

-- primeira versão da group depois de 2 anos sem programar em haskell
group' :: Eq a => [a] -> [[a]]
group' l@(x:xs) = groupAcc l x []

groupAcc :: Eq a => [a] -> a -> [a] -> [[a]]
groupAcc [] _ acc2 = [acc2]
groupAcc l@(x:xs) acc1 acc2
    | x == acc1 = groupAcc xs acc1 (x:acc2)
    | otherwise = acc2 : groupAcc l x []

-- versão melhorada (retirei um dos acumuladores)
group'' :: Eq a => [a] -> [[a]]
group'' [] = [[]]
group'' l@(x:xs) = groupAcc' xs [x]

groupAcc' :: Eq a => [a] -> [a] -> [[a]]
groupAcc' [] acc = [acc]
groupAcc' (x:xs) acc
    | x == (head acc) = groupAcc' xs (x:acc)
    | otherwise = acc : groupAcc' xs  [x]

-- group [1,2,2,3,2] => [[1], [2,2], [3], [2]]

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

-- concat [[1],[2,2],[3],[4,4,4],[5],[4]]
-- [1,2,2,3,4,4,4,5,4]

inits' :: [a] -> [[a]]
inits' l = initsAcc l []

initsAcc :: [a] -> [a] -> [[a]]
initsAcc [] acc = [acc]
initsAcc (x:xs) acc = acc : initsAcc xs (acc ++ [x])

inits'' :: [a] -> [[a]]
inits'' [] = [[]]
inits'' l = inits''(init l) ++ [l]

tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' l@(x:xs) = l : tails' xs 

heads' :: [[a]] -> [a]
heads' [] = []
heads' ([]:t) = heads' t -- é necessário pois head [] dá erro
heads' (x:xs) = (head x) : heads' xs

-- heads [[1,2,3], [], [1,2], []] => head [1,2,3] : heads [[1,2], []] 
-- => [1] : head ([1,2]) : heads [[]] (tail e head [[]] = [])
-- => [1] : [1] : heads [] => [1] : [1] : [] => [1,1]

totalAux :: [a] -> Int
totalAux [] = 0
totalAux (x:xs) = 1 + totalAux xs


total' :: [[a]] -> Int
total' [] = 0
total' (x:xs) = totalAux x + total' xs

-- total [[1,2,3]] => totalAux [1,2,3] + total [] (lista agora sem listas) => 3 + 0 = 3

fun' :: [(a,b,c)] -> [(a,c)]
fun' [] = []
fun' ((a,b,c):xs) = (a,c) : fun' xs

cola' :: [(String, b, c)] -> String
cola' [] = ""
cola' ((a,b,c):xs) = a ++ cola' xs

-- cola' [("rick", 1, 2), ("roll", 1,2)] => "rickroll"

idade' :: Int -> Int -> [(String, Int)] -> [String]
idade' ano idade [] = []
idade' ano idade ((a,b):xs) 
    | ano - b >= idade = a : idade' ano idade xs
    | otherwise = idade' ano idade xs

powerEnumFrom' :: Int -> Int -> [Int]
powerEnumFrom' n m = powerEnumFromAux n m 0

powerEnumFromAux :: Int -> Int -> Int -> [Int]
powerEnumFromAux n m acc
    | acc < m = n^ acc : powerEnumFromAux n m (acc+1)
    | otherwise = []

powerEnumFrom'' :: Int -> Int -> [Int]
-- powerEnumFrom'' n 1 = [1] -- não é necessário se se colocar um m >= 1 em vez de m > 1 
powerEnumFrom'' n m 
    | m >= 1 = powerEnumFrom'' n (m-1) ++ [n^(m-1)]
    | otherwise = []

-- powerEnumFrom 2 3 => powerEnumFrom 2 2 ++ [2^(2)] 
-- => powerEnumFrom 2 1 ++ [2^1] ++ [2^2] => [1] ++ [2] ++ [4] = [1,2,4]

roundedSqrt :: Int -> Int
roundedSqrt n = round $ sqrt $ fromIntegral n

conditionAux :: Int -> Int -> Bool
conditionAux n m 
    | 2 <= m && m <= roundedSqrt n && mod n m == 0 = False
    | m > roundedSqrt n = True
    | otherwise = conditionAux n (m + 1)

isPrime' :: Int -> Bool
isPrime' n = n >= 2 && conditionAux n 2

isPrimeAux :: Int -> Int -> Bool
isPrimeAux n m 
    | n < m * m = True -- sqrt(n) < m => True <=> n < m*m => True
    | mod n m == 0 = False
    | otherwise = isPrimeAux n (m + 1)

isPrime'' :: Int -> Bool
isPrime'' n = n >= 2 && isPrimeAux n 2

-- isPrime 21 = False

isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] _ = True -- esta tem de aparecer primeiro !! isPrefixOf [] [] -> true
isPrefixOf' _ [] = False
isPrefixOf' (x:xs) (y:ys) = x == y && isPrefixOf' xs ys 

isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' [] _ = True
isSuffixOf' _ [] = False
isSuffixOf' l1 l2@(x:xs)
    | l1 == l2 = True
    | otherwise = isSuffixOf' l1 xs

isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' l@(x:xs) (y:ys)
    | x == y = isSubsequenceOf' xs ys
    | otherwise = isSubsequenceOf' l ys

elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' n l = elemIndicesAux n l 0

elemIndicesAux :: Eq a => a -> [a] -> Int -> [Int]
elemIndicesAux _ [] _ = []
elemIndicesAux n (x:xs) acc 
    | n == x = acc : elemIndicesAux n xs (acc + 1)
    | otherwise = elemIndicesAux n xs (acc+1)

find' :: Eq a => a -> [a] -> Bool
find' _ [] = False
find' n (x:xs)
    | n == x = True
    | otherwise = find' n xs


-- Também se poderia fazer esta função com ajuda do `elem` que é igual ao find'
-- Exemplo: 3 `elem` [1,3,2] = True
nubAux :: Eq a => [a] -> [a] -> [a]
nubAux [] acc = acc
nubAux (x:xs) acc
    | find' x acc = nubAux xs acc
    | otherwise = nubAux xs (acc ++ [x])

nub' :: Eq a => [a] -> [a]
nub' l = nubAux l []

delete' :: Eq a => a -> [a] -> [a]
delete' n l  = deleteAux n l []

-- tenho de deixar de fazer acumuladores desnecessários ...
deleteAux :: Eq a => a -> [a] -> [a] -> [a]
deleteAux _ [] acc = acc
deleteAux n (x:xs) acc 
    | n == x = acc ++ xs
    | otherwise = deleteAux n xs (acc ++ [x])

-- versão melhor
delete'' :: Eq a => a -> [a] -> [a]
delete'' n (x:xs)
    | n == x = xs
    | otherwise = x : delete'' n xs

-- delete 2 [1,2,3] => 1 : delete 2 [2,3] => 1 : [3] = [1,3]

slashSlash :: Eq a => [a] -> [a] -> [a]
slashSlash [] l = []
slashSlash l [] = l
slashSlash l1@(x:xs) l2@(y:ys)
    | x == y = slashSlash xs ys
    | otherwise = x : slashSlash xs l2

union' :: Eq a => [a] -> [a] -> [a]
union' l [] = l
union' l (x:xs)
    | x `elem` l = union' l xs
    | otherwise = union' (l ++ [x]) xs 

intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] l = []
intersect' (x:xs) l
    | x `elem` l = x : intersect' xs l
    | otherwise = intersect' xs l

insert' :: Ord a => a -> [a] -> [a]
insert' n [] = [n]
insert' n l@(x:xs)
    | n <= x = n : l
    | otherwise = x : insert' n xs

unwords' :: [String] -> String
unwords' [] = ""
unwords' (x:xs) = x ++ idk ++ unwords' xs
    where idk = case null xs of
                    True -> ""
                    False -> " "

unlines' :: [String] -> String
unlines' [] = "" -- tail ["example"] = []
unlines' (x:xs) = x ++ "\n" ++ unlines' xs


-- 09/10/2023

-- [1,2,3] !! 1 (index) = 2

pMaior' :: Ord a => [a] -> Int
pMaior' l@(x:xs)
    | x == pMaiorAux l = 0
    | otherwise = 1 + pMaior' xs

-- encontra o maior número
pMaiorAux :: Ord a => [a] -> a
pMaiorAux [x] = x 
pMaiorAux (x:y:zs)
    | x >= y = pMaiorAux(x:zs)
    | otherwise = pMaiorAux(y:zs)

lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' _ [] = Nothing
lookup' n ((a,b):xs)
    | n == a = Just b
    | otherwise = lookup' n xs

pMaior'' :: Ord a => [a] -> Int
pMaior'' [_] = 0
pMaior'' (x:xs)
    | x > (xs !! r) = 0
    | otherwise = 1 + r
    where r = pMaior'' xs

preCrescente :: Ord a => [a] -> [a]
preCrescente [] = [] -- apenas para o caso da lista vazia
preCrescente [x] = [x]
preCrescente (x:y:zs) 
    | x < y = x : preCrescente (y:zs)
    | otherwise = [x]

iSort' :: Ord a => [a] -> [a]
isort' [] = []
iSort' [x] = [x]
iSort' (x:xs) = insert' x (iSort' xs)

-- menor em tamanho apenas
menor :: String -> String -> Bool
menor "" "" = False
menor "" _ = True
menor _ "" = False
menor str1 str2 = menor (tail str1) (tail str2)

menor' :: String -> String -> Bool
menor' _ "" = False
menor' "" _ = True
menor' (x:xs) (y:ys)
    | x < y = True
    | x == y = menor' xs ys
    | otherwise = False

elemMSet :: Eq a => a -> [(a, Int)] -> Bool
elemMSet n [] = False
elemMSet n ((a,b):t)
    | n == a = True
    | otherwise = elemMSet n t

converteMSet :: [(a, Int)] -> [a]
converteMSet [] = []
converteMSet ((a,b):t)
    | b > 0 = a : converteMSet ((a,b-1):t)
    | otherwise = converteMSet t

insereMSet :: Eq a => a -> [(a, Int)] -> [(a, Int)]
insereMSet n [] = [(n,1)]
insereMSet n ((a,b):t)
    | n == a = ((a, b+1):t)
    | otherwise = (a,b) : insereMSet n t

removeMSet :: Eq a => a -> [(a, Int)] -> [(a, Int)]
removeMSet _ [] = []
removeMSet n ((a,b):t)
    | n == a && b - 1 == 0 = t
    | n == a && b - 1 > 0 = ((a,b-1):t)
    | otherwise = (a,b) : removeMSet n t

constroiMSet :: Ord a => [a] -> [(a, Int)]
constroiMSet [] = []
constroiMSet (x:xs) = insereMSet x (constroiMSet xs)

-- aux de partitionEithers
insereLeft :: a -> ([a], [b]) -> ([a], [b])
insereLeft x (left, right) = (x : left, right)

-- aux de partitionEithers
insereRight :: b -> ([a], [b]) -> ([a], [b])
insereRight x (left, right) = (left, x : right)

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers [] = ([],[])
partitionEithers (x:xs) = case x of 
                            Left n -> insereLeft n (partitionEithers xs)
                            Right n -> insereRight n (partitionEithers xs)

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) = case x of
                        Just n -> n : catMaybes xs 
                        Nothing -> catMaybes xs
-- catMaybes [Just 1, Just 2, Nothing, Just 3, Nothing] = [1,2,3]

data Movimento = Norte | Sul | Este | Oeste 
    deriving Show

caminho :: (Int, Int) -> (Int, Int) -> [Movimento]
caminho (x1, y1) (x2, y2)
    | x1 /= x2 = if x1 > x2 then Oeste : caminho (x1-1, y1) (x2, y2) else Este : caminho (x1+1, y1) (x2, y2)
    | y1 /= y2 = if y1 > y2 then Sul : caminho (x1, y1-1) (x2, y2) else Norte : caminho (x1, y1+1) (x2, y2) 
    | otherwise = []

hasLoopsAux :: (Int, Int) -> (Int, Int) -> [Movimento] -> Bool
hasLoopsAux _ _ [] = False
hasLoopsAux i (x2, y2) (Norte : t) = i == (x2, y2+1) || hasLoopsAux i (x2, y2 + 1) t
hasLoopsAux i (x2, y2) (Sul : t)   = i == (x2, y2-1) || hasLoopsAux i (x2, y2 - 1) t
hasLoopsAux i (x2, y2) (Este : t)  = i == (x2+1, y2) || hasLoopsAux i (x2 + 1, y2) t
hasLoopsAux i (x2, y2) (Oeste : t) = i == (x2-1, y2) || hasLoopsAux i (x2 - 1, y2) t

hasLoops :: (Int, Int) -> [Movimento] -> Bool
hasLoops (x, y) m = hasLoopsAux (x, y) (x, y) m

type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaQuadrados (x:xs) 
    | eQuadrado x = 1 + contaQuadrados xs
    | otherwise = contaQuadrados xs

eQuadrado :: Rectangulo -> Bool
eQuadrado (Rect (x1,y1) (x2,y2)) = abs (y2 - y1) == abs (x2 - x1)


areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal ((Rect (x1,y1) (x2,y2)):t) = abs (x2 - x1) * abs (y2 - y1) + areaTotal t

data Equipamento = Bom | Razoavel | Avariado
    deriving Show

naoReparar ::  [Equipamento] -> Int
naoReparar [] = 0
naoReparar (x:xs) = case x of
    Avariado -> naoReparar xs
    _ -> 1 + naoReparar xs
