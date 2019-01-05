module ComonadUI.Pairing where

import Control.Comonad (Comonad (..))

type Pairing f g = forall a b c. (a -> b -> c) -> f a -> g b -> c

newtype Co w a = Co { runCo :: forall r. w (a -> r) -> r }

instance Functor w => Functor (Co w) where
    fmap f (Co w) = Co (w . fmap (. f))

instance Comonad w => Monad (Co w) where
    return a = Co $ \w -> extract w a
    Co k >>= f = Co (k . extend (\wa a -> runCo (f a) wa))

instance Comonad w => Applicative (Co w) where
    mf <*> ma = mf >>= \f -> fmap f ma
    pure a = Co (`extract` a)

pairCo :: Functor f => Pairing f (Co f)
pairCo f w cow = runCo cow (fmap f w)

pairCoOp :: Functor f => Pairing (Co f) f
pairCoOp f cow w = pairCo (flip f) w cow



-- applies a monadic transition ( Co w ) to a current state,
-- resulting in a new state
select :: forall w a b. (Functor w) => Co w b -> w (w a) -> w a
select transition current = pairCoOp (const id) transition current

