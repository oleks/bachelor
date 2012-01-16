module Interpreter where

import Syntax
import qualified Parser as Parser
import qualified Text.ParserCombinators.Parsec.Error as Parsec

import Control.Monad
import Data.Map as M

data Context = Context {
    variables :: M.Map String Expression,
    clauses :: M.Map String Clause
  }

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

interpretString :: String -> Either Parsec.ParseError Expression
interpretString programText =
  case (Parser.parseString programText) of
    Left error -> Left error
    Right program -> Right ENil
