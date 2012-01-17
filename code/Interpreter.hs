module Interpreter (interpretString) where

import Syntax
import qualified Parser as Parser
import qualified Text.ParserCombinators.Parsec.Error as Parsec

import Control.Monad
import Data.Map as M

{- begin Type Declarations -}

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

{- Context auxiliaries -}

getContext :: Delta Context
getContext = Delta { runDelta = \context -> (context, context) }

setContext :: Context -> Delta ()
setContext context = Delta { runDelta = \_ -> ((), context) }

{- end Type Declarations -}

{- begin Interpretation -}

interpretString :: String -> Either Parsec.ParseError Expression
interpretString programText =
  case (Parser.parseString programText) of
    Left error -> Left error
    Right program -> Right (interpretProgram program)

interpretProgram :: Program -> Expression
interpretProgram (Program clauses expression) =
  let
    (value, _) = (runDelta (evaluateExpression expression)) (initialContext clauses)
  in
    value

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

{- end Interpretation -}

{- begin Variable Evaluation -}

evaluateVariable :: Name -> [Expression] -> Delta Expression
evaluateVariable name arguments
  = let
      signature = getSignature name arguments
    in
      do
        context <- getContext
        clauses <-
          if (M.member signature context)
          then return $ context M.! signature
          else error $ "undefined variable: " ++ signature
        result <- evaluateClauses clauses arguments
        return result

evaluateClauses :: [StaticClause] -> [Expression] -> Delta Expression
evaluateClauses [] _ = error "pattern matching not exhaustive"
evaluateClauses (head : tail) arguments
  = do
    matched <- evaluateClause head arguments
    case matched of
      Right expression -> return expression
      Left _ -> evaluateClauses tail arguments

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

{- end Variable Evaluation -}

{- begin Pattern Matching -}

evaluatePatternMatches :: [Pattern] -> [Expression] -> Delta Bool
evaluatePatternMatches [] [] = return True
evaluatePatternMatches (p : ps) (x : xs)
  = do
    matched <- evaluatePatternMatch p x
    case matched of
      True -> evaluatePatternMatches ps xs
      _ -> return False
evaluatePatternMatches _ _ = return False

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

{- Variables are bound as 0-ary clauses. -}

makeConstant :: Expression -> [StaticClause]
makeConstant expression = [(StaticClause [] expression M.empty)]

{- end Pattern Matching -}

{- begin Initial Context -}

initialContext :: [Clause] -> Context
initialContext clauses = initializeClauses  clauses M.empty

initializeClauses :: [Clause] -> Context -> Context
initializeClauses [] map = map
initializeClauses ((Clause name patterns expression) : tail) map =
  let
    signature = getSignature name patterns
    clauses = if (M.member signature map)
              then map M.! signature
              else []
    staticClause = StaticClause patterns expression map
  in
    initializeClauses tail (M.insert signature (clauses ++ [staticClause]) map)

{- end Initial Context -}

{- Erlang-like signatures -}

getSignature :: Name -> [t] -> Name
getSignature name t = name ++ ('/' : (show $ length t))













