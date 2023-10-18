module Ex7 where

import Data.List
import Cp

type Key = Int
type Aut = String
type Pag = Int

type Bib = [(Key, [Aut])]
type Aux = [(Pag, [Key])]
type Ind = [(Aut, [Pag])]

-- Solução de Diogo Matos

-- Função Principal - Criar (B x C*)* a partir de (A x B*)* e (C x A*)*
i :: (Eq a, Eq b, Ord b, Ord c) => ([(a, [b])], [(c, [a])]) -> [(b, [c])]
i = sort . map (id >< sort) . h . g . (f >< f)

-- // --

-- Transformar (A x B*)* em (A x B)*
f :: [(a, [b])] -> [(a, b)]
f x = [(a, b) | (a, bs) <- x, b <- bs]
-- (a, bs) <- x -- extracts a and bs from each tuple in the list
-- b <- bs -- iterates over each element b in the list bs
-- (a, b) -- constructs a new tuple where the first element is a (from the outer loop) 
-- and the second element is b (from the inner loop).

-- [expression | generator, qualifier, ...]
-- [x^2 | x <- [1..10], even x]

-- Fazer a junção dos elementos das listas de acordo com as suas chaves (a1 == a2)
g :: Eq a => ([(a, b)], [(c, a)]) -> [(b, c)]
g (x, y) = [(b, c) | (a1, b) <- x, (c, a2) <- y, a1 == a2]

-- Transformar (A x B)* em (A x B*)* (fº)
h :: Eq a => [(a, b)] -> [(a, [b])]
h x = [(a1, [b | (a2, b) <- x, a1 == a2]) | a1 <- y]
    where y = (nub . map fst) x


-- ---------------------------------------------------------------------

-- LISTAS POR COMPREENSÃO (revisão)

-- Na matemática é costume definir conjuntos por compreensão à custa de outros conjuntos. Por exemplo: 
-- { 2x | x ∈ {10,3,7,2}} --> O conjunto {20,6,14,4}
-- { n | n ∈ {4,-5,8,20,-7,1} ∧ 0 <= n <= 10 }  --> O conjunto {4,8,1}

-- Em haskell podem definir-se listas por compreensão, de modo semelhante, contruindo novas listas às custas de outras listas. 
-- `[2*x | x <- [10,3,7,2]]` --> A lista `[20,6,14,4]`
-- `[n   | n <- [4,-5,8,20,-7,1], 0 <= n <= 10]`  --> A lista `[4,8,1]`

-- -> A expressão `n <- [4,-5,8,20,-7,1]` é chamada de gerador da lista. 
-- -> A expressão `0 <= n <= 10` é uma guarda que restringe os valores produzidos pelo gerador que a precede.

-- Assim, as listas por compreensão podem ter vários geradores e várias guardas, sendo que mudar a ordem dos geradores muda a 
-- ordem dos elementos na lista final. Por exemplo: 
-- [(x,y) | x <- [1,2,3], y <- [4,6]]   
-- 	--> [(1,4),(1,6),(2,4),(2,6),(3,4),(3,6)]
-- [(x,y) | y <- [4,6], x <- [1,2,3]]   
-- 	--> [(1,4),(2,4),(3,4),(1,6),(2,6),(3,6)] 

-- qsort :: (Ord a) => [a] -> [a]
-- qsort [] = [] 
-- qsort (x:xs) = (qsort [y | y <- xs, y < x]) ++ [x] ++ (qsort [y | y <- xs, y >= x])

-- divisores :: Integer -> [Integer]
-- divisores n = [x | x <- [1..n], n `mod` x == 0]

-- posicoes :: Eq a => a -> [a] -> [Int]
-- posicoes x l = [i | (y,i) <- zip l [0..], x == y]


-- ---------------------------------------------------------------------

-- FOLDR

-- foldr é uma função de ordem superior que recebe o operador f que é usado para construir o resultado, e o valor z a devolver 
-- quando a lista é vazia. 

-- O seu padrão de computação é semelhante ao das seguintes funções:

-- sum [] = 0 
-- sum (x:xs) = x + (sum xs)

-- product [] = 1 
-- product (x:xs) = x * (product xs)

-- Apenas diferem no operário que é usado e no valor a devolver quando a lista é vazia.

-- foldr :: (a -> b -> b) -> b -> [a] -> b 
-- foldr f z [] = z -- caso de paragem
-- foldr f z (x:xs) = f x (foldr f z xs)

-- Assim, `sum [1,2,3] = foldr (+) 0 [1,2,3]`.
-- Exemplo de ultilização: 

-- reverse l = foldr (\x r -> r++[x]) [] l
-- length = foldr (\h r -> 1+r) 0


-- FOLDL

-- Vai construindo o resultado pelo lado esquerdo da lista.
-- Deste modo, a função foldl sintetiza um padrão de computação que corresponde a trabalhar com um acumulador.
-- O foldl recebe como argumentos a função que combina o acumulador com a cabeça da lista, e o valor inicial do acumulador:

-- foldl :: (b -> a -> b) -> b -> [a] -> b 
-- foldl f z [] = z 
-- foldl f z (x:xs) = foldl f (f z x) xs  

-- --> z é o acumulador.
--      f é usado para combinar o acumulador com a cabeça da lista.
--      (f z x) é o novo valor do acumulador.

-- Exemplos de ultilização: 
-- reverse l = foldl (\ac x -> x:ac) [] l  -- > [] é o valor inicial do acumulador s
-- sum l = foldl (+) 0 l
