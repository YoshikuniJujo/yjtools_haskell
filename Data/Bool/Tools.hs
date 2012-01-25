module Data.Bool.Tools (
  (&&&),
  (|||)
) where

(&&&), (|||) :: Monad m => m Bool -> m Bool -> m Bool
p1 &&& p2 = do b1 <- p1
               b2 <- p2
	       return $ b1 && b2
p1 ||| p2 = do b1 <- p1
               b2 <- p2
	       return $ b1 || b2
