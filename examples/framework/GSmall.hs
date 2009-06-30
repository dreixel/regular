{-# OPTIONS -fglasgow-exts #-}

module GSmall where

import Generics.Regular

-- (Lazy) Peano numbers.

data Size = Zero | Succ Size
  deriving (Eq, Ord, Show)

num Zero     = 0
num (Succ a) = 1 + num a

sizeAdd Zero     b = b
sizeAdd (Succ a) b = Succ (sizeAdd a b)

-- Generic structural size and minimum.

class GSize f where
  gsize' :: (a -> Size) -> f a -> Size

instance GSize U where
  gsize' _ _ = Succ Zero

instance GSize I where
  gsize' f (I r) = Succ (f r)

instance GSize (K a) where
  gsize' f _ = Succ Zero

instance (GSize f, GSize g) => GSize (f :+: g) where
  gsize' f (L x) = gsize' f x
  gsize' f (R y) = gsize' f y

instance (GSize f, GSize g) => GSize (f :*: g) where
  gsize' f (x :*: y) = gsize' f x `sizeAdd` gsize' f y

gsize :: (Regular a, GSize (PF a)) => a -> Size
gsize = gsize' gsize . from

gminimum' :: (GSize f, GSize g) => (a -> Size) -> (b -> Size) -> f a -> g b -> Either (f a) (g b)
gminimum' f g a b = if gsize' f a < gsize' g b then Left a else Right b

gminimum :: (Regular a, GSize (PF a), Regular b, GSize (PF b)) => a -> b -> Either a b
gminimum a b = if gsize a < gsize b then Left a else Right b

-- Small type class for simple types.

class Small a where
  small :: a

instance Small () where
  small = ()

instance Small Bool where
  small = False

instance Small Int where
  small = 0

instance Small Char where
  small = '\NUL'

-- Generic small function. Computes the structural smallest instance of a
-- container certain type.

class GSmall f where
  gsmall' :: (a -> Size) -> a -> f a

instance GSmall U where
  gsmall' _ _ = U

instance GSmall I where
  gsmall' _ f = I f

instance Small a => GSmall (K a) where
  gsmall' _ _ = K small

instance (GSize f, GSize g, GSmall f, GSmall g) => GSmall (f :+: g) where
  gsmall' s f = either L R
    $ gminimum' s s (gsmall' s f) (gsmall' s f)

instance (GSmall f, GSmall g) => GSmall (f :*: g) where
  gsmall' s f = gsmall' s f :*: gsmall' s f

gsmall :: (GSize (PF a), GSmall (PF a), Regular a) => a
gsmall = to $ gsmall' gsize gsmall

