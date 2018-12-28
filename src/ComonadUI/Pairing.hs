module ComonadUI.Pairing where

import Control.Comonad
import Control.Comonad.Trans.Store

type Pairing f g = forall a b. f (a -> b) -> g a -> b

newtype Co w a = Co { runCo :: forall r. w (a -> r) -> r }

pairCo :: Pairing f (Co f)
pairCo f cof = runCo cof f

-- type State s = Co (Store s)

type State s a = s -> (a, s)

get' :: State s s
get' s = (s,s)

put' :: s -> State s ()
put' s = const ((), s)


type Str s a = (s -> a, s)

get :: Co (Store s) s
get = Co $ \w -> extract w (pos w)

state :: (s -> (a,s)) -> Co (Store s) a
state f = Co $ \w -> let (a,s) = f (pos w) in peek s w a

runState :: Co (Store s) a -> s -> (a, s)
runState m s = runCo m (store (\s' a -> (a, s')) s)

-- state :: (s -> (a, s)) -> Co (Store s) a
-- state f = Co $ \st -> extract st

put :: s -> Co (Store s) ()
put s = Co $ \w -> peek s w ()

