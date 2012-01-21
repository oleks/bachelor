module Util where

import Syntax

{- begin Erlang-like signatures -}

getSignature :: Name -> [t] -> Name
getSignature name t = name ++ ('/' : (show $ length t))

{- end Erlang-like signatures -}


