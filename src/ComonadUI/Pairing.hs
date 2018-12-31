module ComonadUI.Pairing where

import Control.Comonad
import Control.Comonad.Trans.Store

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


runState :: Co (Store s) a -> s -> (a, s)
runState m s = runCo m (store (\s' a -> (a, s')) s)

state :: (s -> (a,s)) -> Co (Store s) a
state f = Co $ \w -> let (a,s) = f (pos w) in peek s w a

get :: Co (Store s) s
get = Co $ \w -> extract w (pos w)

put :: s -> Co (Store s) ()
put s = state $ const ((), s)

sf :: Co (Store Integer) [Char]
sf = do
    a <- state (\s -> (s + 1, s))
    b <- fmap (a +) get
    put =<< fmap succ get
    let c = show b
    pure $ "answer: " ++ c


-- selects a particular focus in w using (Co w)
select :: forall w a b. (Functor w) => Co w b -> w (w a) -> w a
select ac co = pairCoOp (const id) ac co


type UI = String
type S = Integer

type Component = Store S
type Action = Co (Store S)



c1 :: Component UI
c1 = store (("v:" ++ ) . show) 100

a0 :: Action ()
a0 = put 900

a1 :: Action ()
a1 = get >>= (\s -> put (s + 10))



c2 :: Component UI
c2 = select a1 (duplicate c1)

c3 :: Component UI
c3 = select a0 (duplicate c1) -- expecting "v:"
