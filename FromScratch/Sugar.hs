
{-# LANGUAGE UnicodeSyntax#-}

data List a = Nil | Cons a (List a) deriving Show




class Count a where
  count :: a



instance Count Int where
  count = 0

instance Count b => Count (a -> b) where
  count a = 1+ count
