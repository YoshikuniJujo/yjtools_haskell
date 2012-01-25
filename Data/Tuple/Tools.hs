module Data.Tuple.Tools (
  modifyFst
, modifySnd
) where

import Control.Arrow (first, second)

modifyFst :: (a -> b) -> (a, c) -> (b, c)
modifyFst = first -- f (x, y) = (f x, y)

modifySnd :: (a -> b) -> (c, a) -> (c, b)
modifySnd = second -- f (x, y) = (x, f y)
