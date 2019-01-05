module ComonadUI.Static where

import Control.Comonad
import Control.Comonad.Env
import Control.Comonad.Identity
import ComonadUI.Pairing


-- Static UI from Env e and Reader e

runReader :: forall a r. r -> Co (Env r) a -> a
runReader r m = runCo m (env r id)

emit :: Co (Env r) r -- same as 'ask' in reader but there is already an ask for env
emit = Co $ \w -> extract w (ask w)


type UI = String
type R = Integer

type Element = Env R
type Emit = Co (Env R)

e1 :: Element UI
e1 = env 0 "Static"

e2 :: Element UI
e2 = select emit (duplicate e1)


-- Static UI from Identity as Comonad

runId :: forall a. Co Identity a -> a
runId m = runCo m (Identity id)

type Element' = Identity
type Emit' = Co Identity

e1' :: Element' UI
e1' = Identity "Static"

e2' :: Element' UI
e2' = select (pure ()) (duplicate e1')


