module Data.Tuple.Tools (
	rotate
) where

rotate :: Floating a => a -> (a, a) -> (a, a)
rotate rad (x, y) = (x * cos rad - y * sin rad, x * sin rad + y * cos rad)
