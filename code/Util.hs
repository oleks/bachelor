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
getVariablesAux PNil names = names
getVariablesAux (PNode leftP rightP) names =
  let
    leftNames = getVariablesAux leftP names
  in
    getVariablesAux rightP leftNames
getVariablesAux (PVariable "_") names = names
getVariablesAux (PVariable name) names = name : names

{- begin Siblings -}

getSiblings :: Pattern -> [Pattern]
getSiblings PNil = [PNode (PVariable "_") (PVariable "_")]
getSiblings (PVariable _) = []
getSiblings (PNode leftP rightP) =
  let
    leftS = getSiblings leftP
    rightS = getSiblings rightP
    leftInit = map (\s -> (PNode leftP s)) rightS
    rightInit = map (\s -> (PNode s rightP)) leftS
  in
    [PNil] ++ leftInit ++ rightInit ++ mergeSiblings leftS rightS rightS

mergeSiblings :: [Pattern] -> [Pattern] -> [Pattern] -> [Pattern]
mergeSiblings [] _ _ = []
mergeSiblings leftS @ (leftH : leftT) (rightH : rightT) rightS =
  (PNode leftH rightH) : (mergeSiblings leftS rightT rightS)
mergeSiblings (_ : leftT) [] rightS =
  mergeSiblings leftT rightS rightS

{- end Siblings -}
