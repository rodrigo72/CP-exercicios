module Ex7 where

import Data.List
import Cp

type Key = Int
type Aut = String
type Pag = Int

type Bib = [(Key, [Aut])]
type Aux = [(Pag, [Key])]
type Ind = [(Aut, [Pag])]

-- Função Principal - Criar (B x C*)* a partir de (A x B*)* e (C x A*)*
i :: (Eq a, Eq b, Ord b, Ord c) => ([(a, [b])], [(c, [a])]) -> [(b, [c])]
i = sort . map (id >< sort) . h . g . (f >< f)

-- // --

-- Transformar (A x B*)* em (A x B)*
f :: [(a, [b])] -> [(a, b)]
f x = [(a, b) | (a, bs) <- x, b <- bs]

-- Fazer a junção dos elementos das listas de acordo com as suas chaves (a1 == a2)
g :: Eq a => ([(a, b)], [(c, a)]) -> [(b, c)]
g (x, y) = [(b, c) | (a1, b) <- x, (c, a2) <- y, a1 == a2]

-- Transformar (A x B)* em (A x B*)* (fº)
h :: Eq a => [(a, b)] -> [(a, [b])]
h x = [(a1, [b | (a2, b) <- x, a1 == a2]) | a1 <- y]
    where y = (nub . map fst) x