module Util(
  getSignature,
  getUnarySignature,
  simplifiedSignature,
  getVariables,
  getSiblings,
  matches,
  matchesShape,
  foldlWithIndex,
  foldlMWithIndex
) where

import Grammar
import Control.Monad
import qualified Data.Foldable as Foldable

{- begin Erlang-like signatures -}

getSignature :: Name -> [t] -> Name
getSignature name t = name ++ ('/' : (show $ length t))

getUnarySignature :: Name -> Name
getUnarySignature name = name ++ "/1"

simplifiedSignature :: Name -> Name
simplifiedSignature name = foldr (\c newName -> if c == '/' then newName else c : newName) [] name

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

matches :: Pattern -> Pattern -> Bool
matches (PNil _) (PNil _) = True
matches (PNil _) _ = False
matches (PVariable _) _ = True
matches _ (PVariable _) = False
matches (PNode _ _ _) (PNil _) = False
matches (PNode _ p11 p12) (PNode _ p21 p22) = matches p11 p21 && matches p12 p22

matchesShape :: Pattern -> Expression -> Bool
matchesShape _ (EVariable _ _) = True
matchesShape (PVariable _) _ = True
matchesShape (PNil _) (ENil _) = True
matchesShape (PNil _) _ = False
matchesShape (PNode _ _ _) (ENil _) = False
matchesShape (PNode _ p1 p2) (ENode _ e1 e2) = matchesShape p1 e1 && matchesShape p2 e2


foldlMWithIndex :: (Foldable.Foldable t, Monad m) => (Int -> a -> b -> m a) -> a -> t b -> m a
foldlMWithIndex function initial list = do
  (_,a) <- Foldable.foldlM
    (\(index,a) b -> do
      result <- function index a b
      return $ (index + 1, result))
    (0, initial)
    list
  return a
