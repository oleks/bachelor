module Util(
  getSignature,
  getUnarySignature,
  getVariables,
  getSiblings
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
getVariablesAux (PNil "_") names = names
getVariablesAux (PNil name) names = name : names
getVariablesAux (PNode name leftP rightP) names =
  let
    leftNames = getVariablesAux leftP names
    nestedNames = getVariablesAux rightP leftNames
  in
    if name == "_" then nestedNames else (name : nestedNames)
getVariablesAux (PVariable "_") names = names
getVariablesAux (PVariable name) names = name : names

{- begin Siblings -}

getSiblings :: Pattern -> [Pattern]
getSiblings (PNil name) = [PNode name (PVariable "_") (PVariable "_")]
getSiblings (PVariable _) = []
getSiblings (PNode name leftP rightP) =
  let
    leftS = getSiblings leftP
    rightS = getSiblings rightP
    leftInit = map (\s -> (PNode name leftP s)) rightS
    rightInit = map (\s -> (PNode name s rightP)) leftS
  in
    [(PNil name)] ++ leftInit ++ rightInit ++ mergeSiblings name leftS rightS rightS

mergeSiblings :: Name -> [Pattern] -> [Pattern] -> [Pattern] -> [Pattern]
mergeSiblings name [] _ _ = []
mergeSiblings name leftS @ (leftH : leftT) (rightH : rightT) rightS =
  (PNode name leftH rightH) : (mergeSiblings name leftS rightT rightS)
mergeSiblings name (_ : leftT) [] rightS =
  mergeSiblings name leftT rightS rightS

{- end Siblings -}
