module ShapeChange where

import Grammar
import qualified Parser as Parser
import qualified Text.ParserCombinators.Parsec.Error as Parsec

import Control.Monad
import Data.Map as M
import Data.Char as C
import System.Process as P

{- begin Type Declarations -}

{-

type Context = M.Map String []

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

-}

{- end Type Declarations -}

data HaltingProperty
  = Halts
  | Unknown
  deriving(Show)

data UnaryClause
  = UnaryClause {
    uClausePattern :: Pattern,
    uClauseExpression :: Expression
  }
instance Show UnaryClause where
  show uClause = (show $ uClausePattern uClause)
    ++ " := " ++ (show $ uClauseExpression uClause)

data UnaryProgram
  = UnaryProgram {
    upClauses :: [UnaryClause],
    upExpression :: Expression
  }
instance Show UnaryProgram where
  show unaryProgram = foldl
    (\partial clause -> (show clause) ++ ('\n' : partial))
    (show $ upExpression unaryProgram)
    (upClauses unaryProgram)

analyzeFile :: String -> IO HaltingProperty
analyzeFile fileName =
  do
    ast <- Parser.parseFile fileName
    analyze ast

analyze :: (Either Parsec.ParseError FunctionProgram) -> IO HaltingProperty
analyze ast =
  case ast of
    Left errorText -> error $ show errorText
    Right program -> analyzeProgram program

analyzeProgram :: FunctionProgram -> IO HaltingProperty
analyzeProgram _ = return Unknown
