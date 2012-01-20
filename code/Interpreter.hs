module Interpreter (
  interpretString,
  interpretFile,
  interpretFileToDot,
  interpretFileToPdf
) where

import Syntax
import qualified Parser as Parser
import qualified Text.ParserCombinators.Parsec.Error as Parsec

import Control.Monad
import Control.Monad.Trans
import Data.Map as M
import Data.Char as C
import Data.Bits as Bits
import System.Process as Process
import System.Random as Random
import System.Time as Time

minInputSize = 1
maxInputSize = 10

{- begin Type Declarations -}

type Frame = [Name]

type Functions = M.Map Name [StaticClause]

type Variables = M.Map Name Expression

data StaticClause = StaticClause [Pattern] Expression Frame
  deriving(Show)

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

emptyContext :: Random.StdGen -> Context
emptyContext stdGen
  = Context {
      contextStdGen = stdGen,
      contextFrame = [],
      contextFunctions = M.empty,
      contextVariables = M.empty
    }


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


setContext :: Context -> Delta ()
setContext context =
  Delta { runDelta = \_ -> ((), context) }

setStdGen :: StdGen -> Delta ()
setStdGen stdGen =
  Delta { runDelta = \context -> ((), context { contextStdGen = stdGen } ) }

setFrame :: Frame -> Delta ()
setFrame frame =
  Delta { runDelta = \context -> ((), context { contextFrame = frame } ) }

setFunctions :: Functions -> Delta ()
setFunctions functions =
  Delta { runDelta = \context -> ((), context { contextFunctions = functions } ) }

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
  in
    do
      dotFileName <- interpretFileToDot fileName
      Process.createProcess $ pdfProcess dotFileName
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

interpret :: (Either Parsec.ParseError Program) -> IO Expression
interpret ast =
  case ast of
    Left errorText -> error $ show errorText
    Right program -> interpretProgram program

interpretProgram :: Program -> IO Expression
interpretProgram (Program clauses expression) = do
  context <- initialContext clauses
  (result, _) <- return $ (runDelta (evaluateExpression expression)) context
  putStrLn $ show result
  return result

evaluateExpression :: Expression -> Delta Expression
evaluateExpression ENil = return ENil
evaluateExpression (ENode e1 e2) = do
    r1 <- evaluateExpression e1
    r2 <- evaluateExpression e2
    return $ ENode r1 r2
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
balancedTree 0 = ENil
balancedTree size =
  let
    halfSize = Bits.shiftR (size - 1) 1
  in
    ENode (balancedTree halfSize) (balancedTree halfSize)

evaluateApplication :: Name -> [Expression] -> Delta Expression
evaluateApplication name expressions = do
  arguments <- mapM evaluateExpression expressions
  variables <- getVariables
  result <-
    case arguments of
      []  ->
        if M.member name variables
        then  evaluateVariable name
        else evaluateCall name arguments
      _   -> evaluateCall name arguments
  return result


{- end Interpretation -}

{- begin Variable Evaluation -}

evaluateVariable :: Name -> Delta Expression
evaluateVariable name = do
  variables <- getVariables
  return $ variables M.! name

evaluateCall :: Name -> [Expression] -> Delta Expression
evaluateCall name arguments
  = let
      signature = getSignature name arguments
    in
      do
        functions <- getFunctions
        clauses <-
          if (M.member signature functions)
          then return $ functions M.! signature
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
evaluateClause (StaticClause patterns expression clauseFrame) arguments =
  do
    originalFrame <- getFrame
    setFrame clauseFrame
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
evaluatePatternMatch PNil ENil = return True
evaluatePatternMatch (PVariable name) expression = do
  variables <- getVariables
  setVariables $ M.insert name expression variables
  return True
evaluatePatternMatch (PNode p1 p2) (ENode e1 e2) = do
  r1 <- evaluatePatternMatch p1 e1
  r2 <- evaluatePatternMatch p2 e2
  return $ r1 && r2
evaluatePatternMatch _ _ = return False

{- end Pattern Matching -}

{- begin Initial Context -}

initialContext :: [Clause] -> IO Context
initialContext clauses =
  do
    picosec <- ctPicosec `liftM` (Time.getClockTime >>= Time.toCalendarTime)
    stdGen <- return $ Random.mkStdGen $ fromInteger $ picosec
    context <- return $ emptyContext stdGen
    return $
      let
        (frame, functions) = initializeFunctions (reverse clauses) [] M.empty
      in
        context { contextFrame = frame, contextFunctions = functions }

initializeFunctions :: [Clause] -> Frame -> Functions -> (Frame, Functions)
initializeFunctions [] frame functions = (frame, ensureExhaustive functions)
initializeFunctions ((Clause name patterns expression) : tail) frame functions =
  let
    signature = getSignature name patterns
    signatureExists = M.member signature functions
    functionFrame =
      if signatureExists
      then frame
      else signature : frame
    staticClause = StaticClause patterns expression functionFrame
    functionClauses = staticClause :
      if signatureExists
      then functions M.! signature
      else []
  in
    initializeFunctions tail functionFrame (M.insert signature functionClauses functions)

ensureExhaustive :: Functions -> Functions
ensureExhaustive functions = mapWithKey ensureExhaustiveFunction functions

ensureExhaustiveFunction :: Name -> [StaticClause] -> [StaticClause]
ensureExhaustiveFunction name clauses = clauses

getSiblings :: Pattern -> [Pattern]
getSiblings PNil = [PNode (PVariable "x") (PVariable "x")]
getSiblings (PVariable _) = []
getSiblings (PNode p1 p2) =
  let
    s1 = getSiblings p1
    s2 = getSiblings p2
    f2 = Prelude.map (\s -> (PNode s p2)) s1
    f1 = Prelude.map (\s -> (PNode p1 s)) s2
  in
    [PNil] ++ f1 ++ f2 ++ mergeSiblings s1 s2 s2

mergeSiblings :: [Pattern] -> [Pattern] -> [Pattern] -> [Pattern]
mergeSublings [] _ _ = []
mergeSiblings s1@(h1 : t1) (h2 : t2) s2 =
  (PNode h1 h2) : (mergeSiblings s1 t2 s2)
mergeSiblings (_ : t1) [] s2 =
  mergeSiblings t1 s2 s2

{- end Initial Context -}

{- begin Erlang-like signatures -}

getSignature :: Name -> [t] -> Name
getSignature name t = name ++ ('/' : (show $ length t))

{- end Erlang-like signatures -}

{- begin Dot Graph Generation -}

data DotGraph
  = DotGraph {
    dotName :: String,
    dotNodes :: String,
    dotEdges :: String
  }

dotGraph :: String -> String -> String -> DotGraph
dotGraph name nodes edges =
  DotGraph {
    dotName = name,
    dotNodes = nodes,
    dotEdges = edges
  }

getNewName :: String -> String
getNewName name
  = case name of
    'z' : tail -> 'a' : 'z' : tail
    head : tail -> (C.chr ((C.ord head) + 1) ) : tail

getDotGraph :: Expression -> String
getDotGraph expression =
  let
    graph = getDotGraphAux "a" expression
    nodes = dotNodes graph
    edges = dotEdges graph
  in
    "digraph G {\n" ++ nodes ++ edges ++ "}"

getDotGraphAux :: String -> Expression -> DotGraph
getDotGraphAux initialName ENil =
  let
    nodes = initialName ++ "[shape=circle,fillcolor=white,label=\"\"];\n"
    edges = []
  in
    dotGraph initialName nodes edges
getDotGraphAux initialName (ENode e1 e2) =
  let
    name1 = getNewName initialName
    graph1 = getDotGraphAux name1 e1
    name2 = getNewName (dotName graph1)
    graph2 = getDotGraphAux name2 e2
    nodes = initialName ++ "[shape=circle,fillcolor=black,style=filled,label=\"\"];\n" ++
      (dotNodes graph1) ++ (dotNodes graph2)
    edges = initialName ++ "->{" ++ name1 ++ (';' : name2) ++
      ('}' : ';' : '\n' : (dotEdges graph1)) ++ (dotEdges graph2)
  in
    dotGraph (dotName graph2) nodes edges
getDorGraphAux _ _ = error "expression wasn't evaluated before drawing"

{- end Dot Graph Generation -}
