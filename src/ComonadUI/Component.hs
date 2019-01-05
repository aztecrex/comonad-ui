module ComonadUI.Component (runState, state, get, put, update, Component, Action, c1, c2, c3, a0, a1) where

import Control.Comonad (extract, duplicate)
import Control.Comonad.Trans.Store

import ComonadUI.Pairing


runState :: Co (Store s) a -> s -> (a, s)
runState m s = runCo m (store (\s' a -> (a, s')) s)

state :: (s -> (a,s)) -> Co (Store s) a
state f = Co $ \w -> let (a,s) = f (pos w) in peek s w a

get :: Co (Store s) s
get = Co $ \w -> extract w (pos w)

put :: s -> Co (Store s) ()
put s = state $ const ((), s)

update :: (s -> s) -> Co (Store s) ()
update = (get >>=) . (put .)


type UI = String
type S = Integer

-- Store and Co Store provide a (most?) general model
-- of UI. It is roughly comparable to React (I think).
-- I believe these can compose via Product and Sum.
-- Hierarchy can be modeled as Comonad Transformer but
-- I am not really happy about that because I expect it
-- results in a static hierarchy encoded in the source.
-- I'd like to explore alternatives to represent
-- sub-component composition.
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
c3 = select a0 (duplicate c1)
