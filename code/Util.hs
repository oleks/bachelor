module Util(
  getSignature,
  getUnarySignature,
  getVariables
) where

import Grammar

{- begin Erlang-like signatures -}

getSignature :: Name -> [t] -> Name
getSignature name t = name ++ ('/' : (show $ length t))

getUnarySignature :: Name -> Name
getUnarySignature name = name ++ "/1"

{- end Erlang-like signatures -}

getVariables :: [Pattern] -> [Name]
getVariables patterns = foldl
  (\list pattern -> getVariablesAux pattern list)
  []
  patterns

getVariablesAux :: Pattern -> [Name] -> [Name]
getVariablesAux PNil names = names
getVariablesAux (PNode leftP rightP) names =
  let
    leftNames = getVariablesAux leftP names
  in
    getVariablesAux rightP leftNames
getVariablesAux (PVariable "_") names = names
getVariablesAux (PVariable name) names = name : names


