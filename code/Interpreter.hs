module Interpreter where

import Syntax
import qualified Parser as Parser
import qualified Text.ParserCombinators.Parsec.Error as Parsec

import Control.Monad
import Data.Map as M

data Shape
  = Node
  | Leaf
  deriving(Show)

type DotPicture = [Shape]

type Context = M.Map String [StaticClause]

data StaticClause = StaticClause [Pattern] Expression Context
  deriving(Show)

data Delta t = Delta {
    runDelta :: Context -> (t, Context)
  }

instance Monad Delta where
  return value = Delta { runDelta = \context -> (value, context) }
  delta >>= f = Delta { runDelta = \context ->
      let
        (value, newContext) = (runDelta delta) context
        newDelta = f value
      in
        (runDelta newDelta) newContext
    }

getContext :: Delta Context
getContext = Delta { runDelta = \context -> (context, context) }

setContext :: Context -> Delta ()
setContext context = Delta { runDelta = \_ -> ((), context) }

getSignature :: Name -> [t] -> Name
getSignature name t = name ++ (show $ length t)

makeConstant :: Expression -> [StaticClause]
makeConstant expression = [(StaticClause [] expression M.empty)]

evaluatePatternMatch :: Pattern -> Expression -> Delta Bool
evaluatePatternMatch PNil ENil = return True
evaluatePatternMatch (PVariable name) expression
  = let
      signature = getSignature name []
      clause = makeConstant expression
    in
      do
        context <- getContext
        setContext $ M.insert signature clause context
        return True
evaluatePatternMatch (PNode p1 p2) (ENode e1 e2)
  = do
    r1 <- evaluatePatternMatch p1 e1
    r2 <- evaluatePatternMatch p2 e2
    return $ r1 && r2
evaluatePatternMatch _ _ = return False

evaluatePatternMatches :: [Pattern] -> [Expression] -> Delta Bool
evaluatePatternMatches [] [] = return True
evaluatePatternMatches (p : ps) (x : xs)
  = do
    matched <- evaluatePatternMatch p x
    case matched of
      True -> evaluatePatternMatches ps xs
      _ -> return False
evaluatePatternMatches _ _ = return False

evaluateClause :: StaticClause -> [Expression] -> Delta (Either () Expression)
evaluateClause (StaticClause patterns expression clauseContext) arguments
  = let
      (matched, boundContext) = (runDelta (evaluatePatternMatches patterns arguments)) clauseContext
    in
      case matched of
        True ->
          let
            (value, _) = (runDelta (evaluateExpression expression)) boundContext
          in
            return $ Right value
        _ -> return $ Left ()

evaluateClauses :: [StaticClause] -> [Expression] -> Delta Expression
evaluateClauses [] _ = return ENil
evaluateClauses (head : tail) arguments
  = do
    matched <- evaluateClause head arguments
    case matched of
      Right expression -> return expression
      Left _ -> evaluateClauses tail arguments

evaluateVariable :: Name -> [Expression] -> Delta Expression
evaluateVariable name arguments
  = do
    context <- getContext
    signature <- return $ name ++ (show $ length arguments)
    clauses <- return $ context M.! signature
    result <- evaluateClauses clauses arguments
    return result

evaluateExpression :: Expression -> Delta Expression
evaluateExpression ENil = return ENil
evaluateExpression (ENode e1 e2)
  = do
    r1 <- evaluateExpression e1
    r2 <- evaluateExpression e2
    return $ ENode r1 r2
evaluateExpression (EVariable name expressions)
  = do
    arguments <- mapM evaluateExpression expressions
    result <- evaluateVariable name arguments
    return result

interpretExpression :: DotPicture -> Expression -> Delta DotPicture
interpretExpression dotPicture ENil = return (Leaf : dotPicture)
interpretExpression dotPicture (ENode e1 e2)
  = do
    d1 <- interpretExpression dotPicture e1
    d2 <- interpretExpression d1 e2
    return d2
interpretExpression dotPicture variable
  = do
    value <- evaluateExpression variable
    d <- interpretExpression dotPicture value
    return d

interpretProgram :: Program -> DotPicture
interpretProgram (Program clauses expression) =
  let
    (value, _) = (runDelta (interpretExpression [] expression)) (initialContext clauses)
  in
    value

initialContext :: [Clause] -> Context
initialContext clauses = initializeClauses  clauses M.empty

initializeClauses :: [Clause] -> Context -> Context
initializeClauses [] map = map
initializeClauses ((Clause name patterns expression) : tail) map =
  let
    signature = name ++ (show $ length patterns)
    clauses = if (M.member signature map)
              then map M.! signature
              else []
    staticClause = StaticClause patterns expression map
  in
    initializeClauses tail (M.insert signature (clauses ++ [staticClause]) map)

interpretString :: String -> Either Parsec.ParseError DotPicture
interpretString programText =
  case (Parser.parseString programText) of
    Left error -> Left error
    Right program -> Right (interpretProgram program)
