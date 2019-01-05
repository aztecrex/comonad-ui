module ComonadUI.Static where

import Control.Comonad
import Control.Comonad.Env
import ComonadUI.Pairing


runReader :: forall a r. r -> Co (Env r) a -> a
runReader r w = runCo w (env r id)

emit :: Co (Env r) r
emit = Co $ \w -> extract w (ask w)


type UI = String
type R = Integer

type Element = Env R
type Emit = Co (Env R)

e1 :: Element UI
e1 = env 0 "Static"

e2 :: Element UI
e2 = select emit (duplicate e1)




