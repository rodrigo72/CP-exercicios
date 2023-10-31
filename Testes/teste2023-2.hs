module Teste2023_2 where

import Cp

in' :: Either () a -> Maybe a
in' = either (const Nothing) Just

out' :: Maybe a -> Either () a
out' = maybe (i1 ()) i2

out'' :: Maybe a -> Either () a
out'' Nothing = i1 ()
out'' (Just a) = i2 a

-- outMaybe recebe apenas o 'Maybe a', pois o 'a' não é pointwise, fica nos argumentos
fromMaybe :: a -> Maybe a -> a
fromMaybe a = either (const a) id . outMaybe
-- either :: (a->c) -> (b->c) -> Either a b -> c
-- outMaybe :: Maybe b        -> Either () b

fromMaybe' a Nothing = a
fromMaybe' a (Just a') = a'

