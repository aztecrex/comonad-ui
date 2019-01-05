module ComonadUI.Static where

import Control.Comonad
import Control.Comonad.Env
import Control.Comonad.Identity
import ComonadUI.Pairing

import Data.Text (Text, pack)

-- Static UI from Env e and Reader e
-- What is this intuitively and how does it differ
--   from the Identity version below?

runReader :: forall a r. r -> Co (Env r) a -> a
runReader r m = runCo m (env r id)

cask :: Co (Env r) r -- same as 'ask' in reader but there is already an ask for env
cask = Co $ \w -> extract w (ask w)

reader :: (r -> a) -> Co (Env r) a
reader f = Co $ \w -> extract w (f (ask w))

type UI = String
type R = Integer

type Element = Env R
type Emit = Co (Env R)

e1 :: Element UI
e1 = env 0 "Static"

noop :: Emit ()
noop = pure ()


plus1 :: Emit Text -- type parameter doesn't matter, nor does the read function
plus1 = reader (pack . show . (+ 1))

e2 :: Element UI
e2 = select noop (duplicate e1) -- "Static"

e3 :: Element UI
e3 = select plus1 (duplicate e1) -- "Static"




-- Static UI from Identity as Comonad

runId :: forall a. Co Identity a -> a
runId m = runCo m (Identity id)

type Element' = Identity
type Emit' = Co Identity

e1' :: Element' UI
e1' = Identity "Static"

noop' :: Emit' ()
noop' = pure ()

nomatter' :: Emit' Int -- type parameter doesn't matter, nor does the value
nomatter' = pure 1000

e2' :: Element' UI
e2' = select noop' (duplicate e1') -- "Static"

e3' :: Element' UI
e3' = select nomatter' (duplicate e1') -- "Static"
