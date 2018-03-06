module Data.Validated where
import Data.Semigroup

data Validated e a = Valid a | Invalid e

instance Functor (Validated e) where
  fmap f (Invalid e) = Invalid e
  fmap f (Valid a) = Valid $ f a

instance (Semigroup e) => Applicative (Validated e) where
  pure = Valid
  Valid f <*> Valid a = Valid $ f a
  Valid f <*> Invalid e = Invalid e
  Invalid e1 <*> Invalid e2 = Invalid $ e1 <> e2
  Invalid e1 <*> Valid a = Invalid e1