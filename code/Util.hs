module Util where

import Syntax

{- begin Erlang-like signatures -}

getSignature :: Name -> [t] -> Name
getSignature name t = name ++ ('/' : (show $ length t))

getUnarySignature :: Name -> Name
getUnarySignature name = name ++ "/1"

{- end Erlang-like signatures -}

getVariableNames :: Pattern -> [Name] -> [Name]
getVariableNames PNil names = names
getVariableNames (PNode leftP rightP) names =
  let
    leftNames = getVariableNames leftP names
  in
    getVariableNames rightP leftNames
getVariableNames (PVariable "_") names = names
getVariableNames (PVariable name) names = name : names


