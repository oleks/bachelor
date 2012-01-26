module Interpreter (
  interpretString,
  interpretFile,
  interpretFileToDot,
  interpretFileToPdf
) where

import Grammar
import Util
import DotGraph
import qualified Parser as Parser
import qualified Text.ParserCombinators.Parsec.Error as Parsec

import Control.Monad
import Control.Monad.Trans
import qualified Data.Map as Map
import qualified Data.Char as Char
import qualified Data.Bits as Bits
import qualified System.Process as Process
import qualified System.Random as Random
import qualified System.Time as Time

minInputSize = 1
maxInputSize = 10

{- begin Type Declarations -}

type Variables = Map.Map Name Expression

data Context
  = Context
  {
    contextStdGen :: Random.StdGen,
    contextFrame :: Frame,
    contextFunctions :: Functions,
    contextVariables :: Variables
  }
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

{- end Type Declarations -}

{- begin Context Auxiliaries -}

getContext :: Delta Context
getContext =
  Delta { runDelta = \context -> (context, context) }

getFromContext :: (Context -> t) -> Delta t
getFromContext f =
  Delta { runDelta = \context -> ((f context), context) }

getStdGen :: Delta (Random.StdGen)
getStdGen = getFromContext contextStdGen

getFrame :: Delta Frame
getFrame = getFromContext contextFrame

getFunctions :: Delta Functions
getFunctions = getFromContext contextFunctions

getVariables :: Delta Variables
getVariables = getFromContext contextVariables


setStdGen :: Random.StdGen -> Delta ()
setStdGen stdGen =
  Delta { runDelta = \context -> ((), context { contextStdGen = stdGen } ) }

setFrame :: Frame -> Delta ()
setFrame frame =
  Delta { runDelta = \context -> ((), context { contextFrame = frame } ) }

setVariables :: Variables -> Delta ()
setVariables variables =
  Delta { runDelta = \context -> ((), context { contextVariables = variables } ) }

{- end Context Auxiliaries -}

{- begin Interpretation -}

{- dot -Tpdf f.delta.dot -o f.delta.pdf -}

interpretFileToPdf :: String -> IO String
interpretFileToPdf fileName =
  let
    pdfFileName = fileName ++ ".pdf"
    pdfProcess = \dotFileName -> Process.proc "dot" ["-Tpdf", dotFileName, "-o", pdfFileName]
    convertProcess = Process.proc "convert" ["-density", "300", pdfFileName, "-negate", pdfFileName]
  in
    do
      dotFileName <- interpretFileToDot fileName
      (_,_,_,h) <- Process.createProcess $ pdfProcess dotFileName
      Process.waitForProcess h
      (_,_,_,h) <- Process.createProcess $ convertProcess
      Process.waitForProcess h
      return pdfFileName

interpretFileToDot :: String -> IO String
interpretFileToDot fileName =
  let
    dotFileName = fileName ++ ".dot"
  in
    do
      result <- interpretFile fileName
      writeFile dotFileName $ getDotGraph $ result
      return dotFileName

interpretFile :: String -> IO Expression
interpretFile fileName =
  do
    ast <- Parser.parseFile fileName
    result <- interpret ast
    return result

interpretString :: String -> IO Expression
interpretString programText = interpret (Parser.parseString programText)

interpret :: (Either Parsec.ParseError FunctionProgram) -> IO Expression
interpret ast =
  case ast of
    Left errorText -> error $ show errorText
    Right program -> interpretProgram program

interpretProgram :: FunctionProgram -> IO Expression
interpretProgram functionProgram =
  let
    functions = fpFunctions functionProgram
    frame = Map.keys functions
    expression = fpExpression functionProgram
  in
    do
      context <- initialContext frame functions
      (result, _) <- return $ (runDelta (evaluateExpression expression)) context
      putStrLn $ show result
      return result

evaluateExpression :: Expression -> Delta Expression
evaluateExpression (ENil name) = return (ENil name)
evaluateExpression (ENode name e1 e2) = do
    r1 <- evaluateExpression e1
    r2 <- evaluateExpression e2
    return $ ENode name r1 r2
evaluateExpression (EVariable name expressions)
  = if name == "input"
    then evaluateInput
    else evaluateApplication name expressions

evaluateInput :: Delta Expression
evaluateInput = do
  stdGen <- Interpreter.getStdGen
  (size, newStdGen) <- return $ Random.randomR (minInputSize,maxInputSize) stdGen
  Interpreter.setStdGen newStdGen
  return $ balancedTree size

balancedTree :: Int -> Expression
balancedTree 0 = (ENil "_")
balancedTree size =
  let
    halfSize = Bits.shiftR (size - 1) 1
  in
    ENode "_" (balancedTree halfSize) (balancedTree halfSize)

evaluateApplication :: Name -> [Expression] -> Delta Expression
evaluateApplication name expressions = do
  arguments <- mapM evaluateExpression expressions
  variables <- Interpreter.getVariables
  result <-
    case arguments of
      []  ->
        if Map.member name variables
        then  evaluateVariable name
        else evaluateCall name arguments
      _   -> evaluateCall name arguments
  return result

{- end Interpretation -}

{- begin Variable Evaluation -}

evaluateVariable :: Name -> Delta Expression
evaluateVariable name = do
  variables <- Interpreter.getVariables
  return $ variables Map.! name

evaluateCall :: Name -> [Expression] -> Delta Expression
evaluateCall name arguments
  = let
      signature = getSignature name arguments
    in
      do
        functions <- getFunctions
        clauses <-
          if (Map.member signature functions)
          then return $ functions Map.! signature
          else error $ "Interpreter: undefined variable: " ++ signature
        result <- evaluateClauses clauses arguments
        return result

evaluateClauses :: [FunctionClause] -> [Expression] -> Delta Expression
evaluateClauses [] _ = error "pattern matching not exhaustive"
evaluateClauses (head : tail) arguments
  = do
    matched <- evaluateClause head arguments
    case matched of
      Right expression -> return expression
      Left _ -> evaluateClauses tail arguments

evaluateClause :: FunctionClause -> [Expression] -> Delta (Either () Expression)
evaluateClause functionClause arguments =
  let
    patterns = fClausePatterns functionClause
    expression = fClauseExpression functionClause
    frame = fClauseFrame functionClause
  in
    do
      originalFrame <- getFrame
      setFrame frame
      matched <- evaluatePatternMatches patterns arguments
      if matched
      then
        do
          result <- evaluateExpression expression
          setFrame originalFrame
          return $ Right result
      else
        return $ Left ()

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
evaluatePatternMatch (PNil "_") (ENil _) = return True
evaluatePatternMatch (PNil name) (ENil n1) = do
  bindVariable name (ENil n1)
  return True
evaluatePatternMatch (PVariable name) expression = do
  bindVariable name expression
  return True
evaluatePatternMatch (PNode name p1 p2) (ENode n1 e1 e2) = do
  r1 <- evaluatePatternMatch p1 e1
  r2 <- evaluatePatternMatch p2 e2
  if r1 && r2
  then  if name == "_"
        then return True
        else do
          bindVariable name (ENode n1 e1 e2)
          return True
  else return False
evaluatePatternMatch _ _ = return False

bindVariable :: Name -> Expression -> Delta ()
bindVariable name expression = do
  variables <- Interpreter.getVariables
  setVariables $ Map.insert name expression variables

{- end Pattern Matching -}

{- begin Initial Context -}

initialContext :: Frame -> Functions -> IO Context
initialContext frame functions = do
  picosec <- Time.ctPicosec `liftM` (Time.getClockTime >>= Time.toCalendarTime)
  stdGen <- return $ Random.mkStdGen $ fromInteger $ picosec
  return $
    Context {
      contextStdGen = stdGen,
      contextFrame = frame,
      contextFunctions = functions,
      contextVariables = Map.empty
    }

{- end Initial Context -}
