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
import Data.Map as M
import Data.Char as C
import System.Process as Process
import System.Random as Random

{- begin Type Declarations -}

type Variables = M.Map String Int
type Clauses = M.Map String StaticClause

data Context
  = Context
  {
    contextVariables :: Variables,
    contextClauses :: Clauses,
    contextStdGen :: Random.StdGen
  }
  deriving(Show)

data StaticClause
  = StaticClause [Pattern] Expression [String]
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

makeContext :: Random.StdGen -> M.Map

getContext :: Delta Context
getContext =
  Delta { runDelta = \context -> (context, context) }

getFromContext :: (Context -> t) -> Delta t
getFromContext f =
  Delta { runDelta = \context -> ((f context), context) }

getVariables :: Delta (M.Map String Int)
getVariables = getFromContext contextVariables

getClauses :: Delta (M.Map String StaticClause)
getClauses = getFromContext contextClauses

getStdGen :: Delta (Random.StdGen)
getStdGen = getFromContext contextStdGen

setContext :: Context -> Delta ()
setContext context =
  Delta { runDelta = \_ -> ((), context) }

setVariables :: M.Map String Int -> Delta ()
setVariables newVariables =
  Delta { runDelta = \context -> ((), context { contextVariables = newVariables } ) }

{- end Context Auxiliaries -}

{- begin Interpretation -}

{- dot -Tpdf f.delta.dot -o f.delta.pdf -}

interpretFileToPdf :: String -> IO String
interpretFileToPdf fileName =
  let
    pdfFileName = fileName ++ ".pdf"
    pdfProcess = \dotFileName -> Process.proc "dot" ["-Tpdf", dotFileName, "-o", pdfFileName]
  in
    return ""
{-
    do
      dotFileName <- interpretFileToDot fileName
      Process.createProcess $ pdfProcess dotFileName
      return pdfFileName
-}

interpretFileToDot :: String -> IO String
interpretFileToDot fileName =
  let
    dotFileName = fileName ++ ".dot"
  in
    return ""
{-
    do
      result <- interpretFile fileName
      writeFile dotFileName $ getDotGraph $ result
      return dotFileName
-}

interpretFile :: String -> IO Context
interpretFile fileName =
  do
    ast <- Parser.parseFile fileName
    result <- interpret ast
    return result

interpretString :: String -> IO Context
interpretString programText = interpret (Parser.parseString programText)

interpret :: (Either Parsec.ParseError Program) -> IO Context
interpret ast =
  case ast of
    Left errorText -> error $ show errorText
    Right program -> interpretProgram program

interpretProgram :: Program -> IO Context
interpretProgram (Program clauses expression) =
  do
    context <- initialContext clauses
    return context
{-
    return $
      let
        (result, _) = (runDelta (evaluateExpression expression)) context
      in
        result
-}

{-

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
        variables <- getVariables
        clauses <-
          if (M.member signature variables)
          then return $ variables M.! signature
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
evaluateClause (StaticClause patterns expression clauseVariables) arguments =
  do
    originalVariables <- getVariables
    setVariables clauseVariables
    matched <- evaluatePatternMatches patterns arguments
    if matched
    then
      do
        result <- evaluateExpression expression
        setVariables originalVariables
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
evaluatePatternMatch (PVariable name) expression
  = let
      signature = getSignature name []
    in
      do
        clause <- makeConstant expression
        variables <- getVariables
        setVariables $ M.insert signature clause variables
        return True
evaluatePatternMatch (PNode p1 p2) (ENode e1 e2)
  = do
    r1 <- evaluatePatternMatch p1 e1
    r2 <- evaluatePatternMatch p2 e2
    return $ r1 && r2
evaluatePatternMatch _ _ = return False

{- Variables are bound as 0-ary clauses. -}

makeConstant :: Expression -> Delta [StaticClause]
makeConstant expression = return [(StaticClause [] expression M.empty)]

{- end Pattern Matching -}

{- begin Initial Context -}

-}

initialContext :: [Clause] -> IO Context
initialContext clauses =
  do
    stdGen <- Random.getStdGen
    context <- return $ makeContext stdGen M.empty M.empty
    return $
      let
        finalVariables = initializeClauses clauses context
        finalClauses = mapWithKey (normalizeClause clauses) finalVariables
      in
        context { contextVariable contextClauses = finalClauses }

initializeClauses :: [Clause] -> Variables -> M.Map String Int
initializeClauses [] variables = variables
initializeClauses ((Clause name patterns expression) : tail) variables =
  let
    signature = getSignature name patterns
    signatureExists = M.member signature variables
    clauses =
      if signatureExists
      then variables M.! signature
      else []
    staticClause = StaticClause patterns expression variables
  in
    initializeClauses tail (M.insert signature (clauses ++ [staticClause]) variables)

normalizeClause :: Variables -> String -> [StaticClause] -> [StaticClause]
normalizeClause initialVariables name clauses
  = Prelude.map (\clause -> normalizeStaticClause name initialVariables clause) clauses

normalizeStaticClause :: String -> Variables -> StaticClause -> StaticClause
normalizeStaticClause name initialVariables (StaticClause patterns expression variables) =
  let
    updateClauses = \key _ acc -> M.insert key (initialVariables M.! key) acc
    fixPoint = M.fromList [(name, (initialVariables M.! name))]
    newVariables = M.foldrWithKey (updateClauses) fixPoint variables
  in
    StaticClause patterns expression newVariables

{- end Initial Context -}

{- begin Erlang-like signatures -}

getSignature :: Name -> [t] -> Name
getSignature name t = name ++ ('/' : (show $ length t))

{- end Erlang-like signatures -}

{- begin Dot Graph Generation -}

{- main node name, node text, edge text -}

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
    dotGraph name2 nodes edges
getDorGraphAux _ _ = error "expression wasn't evaluated before drawing"

{- end Dot Graph Generation -}
