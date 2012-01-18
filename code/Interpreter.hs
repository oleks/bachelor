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
import System.Process as P

{- begin Type Declarations -}

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

{- end Type Declarations -}

{- begin Context Auxiliaries -}

getContext :: Delta Context
getContext = Delta { runDelta = \context -> (context, context) }

setContext :: Context -> Delta ()
setContext context = Delta { runDelta = \_ -> ((), context) }

{- end Context Auxiliaries -}

{- begin Interpretation -}

{- dot -Tpdf f.delta.dot -o f.delta.pdf -}

interpretFileToPdf :: String -> IO String
interpretFileToPdf fileName =
  let
    pdfFileName = fileName ++ ".pdf"
  in
    do
      dotFileName <- interpretFileToDot fileName
      P.createProcess (P.proc "dot" ["-Tpdf", dotFileName, "-o", pdfFileName])
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
    return $ interpret ast

interpretString :: String -> Expression
interpretString programText = interpret (Parser.parseString programText)

interpret :: (Either Parsec.ParseError Program) -> Expression
interpret ast =
  case ast of
    Left errorText -> error $ show errorText
    Right program -> interpretProgram program

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
