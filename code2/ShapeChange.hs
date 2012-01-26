module ShapeChange where

import Grammar
import Util
import DotGraph
import qualified Parser as Parser
import qualified Text.ParserCombinators.Parsec.Error as Parsec

import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Foldable as Foldable

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

{- begin Call Graph Context -}

data CallGraphContext
  = CallGraphContext {
    cgcFunctions :: Functions,
    cgcGraph :: ShapeGraph
  }
  deriving(Show)

cgcGet :: (CallGraphContext -> t) -> State CallGraphContext t
cgcGet f = do
  context <- get
  return $ f context

cgcGetFunctions :: State CallGraphContext Functions
cgcGetFunctions = cgcGet cgcFunctions

cgcGetGraph :: State CallGraphContext ShapeGraph
cgcGetGraph = cgcGet cgcGraph

cgcPutGraph :: ShapeGraph -> State CallGraphContext ()
cgcPutGraph shapeGraph = do
  context <- get
  put $ context { cgcGraph = shapeGraph }

cgcPutCall :: ShapeEdge -> State CallGraphContext ()
cgcPutCall shapeEdge = do
  context <- get
  put $ context { cgcGraph = shapeEdge : (cgcGraph context) }

type VariablePatterns = Map.Map Name Pattern

data ClauseContext
  = ClauseContext {
    ccSourceSignature :: ClauseSignature,
    ccVariablePatterns :: VariablePatterns,
    ccGraphContext :: CallGraphContext
  }
  deriving(Show)

ccGet :: (ClauseContext -> t) -> State ClauseContext t
ccGet f = do
  context <- get
  return $ f context

ccGetVariablePatterns :: State ClauseContext VariablePatterns
ccGetVariablePatterns = ccGet ccVariablePatterns

ccGetVariablePattern :: Name -> State ClauseContext Pattern
ccGetVariablePattern name = do
  variablePatterns <- ccGetVariablePatterns
  return $ variablePatterns Map.! name

isBoundVariable :: Name -> State ClauseContext Bool
isBoundVariable name = do
  variablePatterns <- ccGetVariablePatterns
  return $ Map.member name variablePatterns

ccGetClauses :: FunctionSignature -> State ClauseContext [FunctionClause]
ccGetClauses functionSignature = do
  callGraphContext <- ccGet ccGraphContext
  return $
    let
      functions = cgcFunctions callGraphContext
    in
      functions Map.! functionSignature

ccPutCall :: (ClauseSignature -> ShapeEdge) -> State ClauseContext ()
ccPutCall f = do
  context <- get
  put $
    let
      sourceSignature = ccSourceSignature context
      shapeEdge = f sourceSignature
      callGraphContext = ccGraphContext context
      shapeGraph = cgcGraph callGraphContext
      newGraphContext = callGraphContext { cgcGraph = shapeEdge : shapeGraph }
    in
      context { ccGraphContext = newGraphContext }

{- begin Analysis Initialization -}

analyzeFileToDot :: String -> IO String
analyzeFileToDot fileName = do
  shapeGraph <- analyzeFile fileName
  return $ getShapeGraph shapeGraph

analyzeFile :: String -> IO ShapeGraph
analyzeFile fileName = do
  ast <- Parser.parseFile fileName
  case ast of
    Left errorText -> error $ show errorText
    Right program -> analyzeProgram program

analyzeProgram :: FunctionProgram -> IO ShapeGraph
analyzeProgram functionProgram =
  let
    functions = Map.map (reverse) (fpFunctions functionProgram)
    initialContext = CallGraphContext functions []
    finalContext = execState (analyzeFunctions functions) initialContext
  in
    return (cgcGraph finalContext)

{- end Analysis Initialization -}

analyzeFunctions :: Functions -> State CallGraphContext ()
analyzeFunctions functions = Foldable.foldlM
  (\_ (functionSignature, functionClauses) -> foldlMWithIndex
    (\clauseIndex _ functionClause ->
      let clauseSignature = getClauseSignature functionSignature clauseIndex
      in analyzeClause clauseSignature functionClause)
    ()
    functionClauses)
  ()
  (Map.toList functions)

analyzeClause :: ClauseSignature -> FunctionClause -> State CallGraphContext ()
analyzeClause clauseSignature functionClause = do
  callGraphContext <- get
  put $
    let
      patterns = fClausePatterns functionClause
      expression = fClauseExpression functionClause

      variablePatterns = getAllVariablePatterns patterns
      initialContext =
        ClauseContext clauseSignature variablePatterns callGraphContext
      finalContext =
        execState (analyzeExpression expression) initialContext
    in
      ccGraphContext finalContext

getAllVariablePatterns :: [Pattern] -> VariablePatterns
getAllVariablePatterns patterns = foldl (getVariablePatterns) Map.empty patterns

getVariablePatterns :: VariablePatterns -> Pattern -> VariablePatterns
getVariablePatterns variablePatterns (PNil "_") = variablePatterns
getVariablePatterns variablePatterns (PVariable "_") = variablePatterns
getVariablePatterns variablePatterns (PNil name) =
  Map.insert name (PNil name) variablePatterns
getVariablePatterns variablePatterns (PVariable name) =
  Map.insert name (PVariable name) variablePatterns
getVariablePatterns variablePatterns (PNode "_" p1 p2) =
  let
    vp1 = getVariablePatterns variablePatterns p1
    vp2 = getVariablePatterns variablePatterns p2
  in
    vp2
getVariablePatterns variablePatterns (PNode name p1 p2) =
  Map.insert name (PNode name p1 p2) variablePatterns

{- Note: There's supposedly no need to delve deaper in the last clause. -}

analyzeExpression :: Expression -> State ClauseContext ()
analyzeExpression (ENil _) = return ()
analyzeExpression (ENode _ e1 e2) = do
  analyzeExpression e1
  analyzeExpression e2
  return ()
analyzeExpression (EVariable name []) = do
  isVariable <- isBoundVariable name
  if isVariable
  then return ()
  else analyzeCall (getSignature name []) []
analyzeExpression (EVariable name arguments) = do
  analyzeCall (getSignature name arguments) arguments

analyzeCall :: FunctionSignature -> Arguments -> State ClauseContext ()
analyzeCall functionSignature arguments = do
  clauses <- ccGetClauses functionSignature
  pureArguments <- mapM (purifyExpression) arguments
  foldlMWithIndex
    (\index _ functionClause ->
      let clauseSignature = getClauseSignature functionSignature index
      in matchClause clauseSignature functionClause pureArguments)
    ()
    clauses
  return ()

purifyExpression :: Expression -> State ClauseContext Expression
purifyExpression (ENil name) = return (ENil name)
purifyExpression (ENode name e1 e2) = do
  d1 <- purifyExpression e1
  d2 <- purifyExpression e2
  return $ ENode name d1 d2
purifyExpression (EVariable name []) = do
  isVariable <- isBoundVariable name
  if isVariable
  then do
    variablePattern <- ccGetVariablePattern name
    return $ patternToExpression variablePattern
  else return $ EVariable "_" []

patternToExpression :: Pattern -> Expression
patternToExpression (PNil name) = (ENil name)
patternToExpression (PVariable name) = EVariable name []
patternToExpression (PNode name p1 p2) =
  let
    e1 = patternToExpression p1
    e2 = patternToExpression p2
  in
    (ENode name e1 e2)

matchClause :: ClauseSignature -> FunctionClause -> [Expression] -> State ClauseContext ()
matchClause clauseSignature functionClause arguments =
  let
    patterns = fClausePatterns functionClause
    allMatch = all (\(pattern, shape) -> matchesShape pattern shape) (zip patterns arguments)
  in
    if allMatch
    then do
      mapM (deduceRelation clauseSignature) (zip patterns arguments)
      return ()
    else return ()

makeShapeEdge :: ClauseSignature -> Name -> Name -> ShapeChange -> ClauseSignature -> ShapeEdge
makeShapeEdge targetSignature targetVariable sourceVariable shapeChange sourceSignature =
  ShapeEdge sourceSignature targetSignature sourceVariable targetVariable shapeChange

deduceRelation :: ClauseSignature -> (Pattern, Expression) -> State ClauseContext ()

{-

deduceRelation targetSignature (pattern, expression) = do
  ccPutCall (makeShapeEdge targetSignature "_" "_" UnknownChange)
  error $ show (pattern, expression)

-}

deduceRelation _ (_, (ENil _))                = return ()
deduceRelation _ ((PNil _), _)            = return ()

{- if pattern is Nil, then expression must be Nil as well, hence no relation. -}

deduceRelation _ (p, (EVariable "_" _))   = return ()
deduceRelation _ ((PVariable "_"), e)     = return ()

deduceRelation targetSignature (PVariable targetName, EVariable sourceName _) =
  ccPutCall (makeShapeEdge targetSignature targetName sourceName LessOrEqual)

deduceRelation targetSignature (PNode "_" p1 p2, ENode "_" e1 e2) = do
  deduceRelation targetSignature (p1,e1)
  deduceRelation targetSignature (p2,e2)

deduceRelation targetSignature (pattern @ (PNode name p1 p2), ENode sourceName _ _) =
  let
    patternVariables = getVariables [p1,p2]
  in do
    Foldable.foldlM
      (\_  targetName -> ccPutCall
        (makeShapeEdge targetSignature targetName sourceName Less))
      ()
      patternVariables
    if name /= "_"
    then ccPutCall (makeShapeEdge targetSignature name sourceName LessOrEqual)
    else return ()

deduceRelation targetSignature (pattern @ (PNode name p1 p2), EVariable sourceName _) =
  let
    patternVariables = getVariables [p1,p2]
  in do
    Foldable.foldlM
      (\_  targetName -> ccPutCall
        (makeShapeEdge targetSignature targetName sourceName Less))
      ()
      patternVariables
    if name /= "_"
    then ccPutCall (makeShapeEdge targetSignature name sourceName LessOrEqual)
    else return ()



deduceRelation targetSignature (pattern, expression) = do
  patternVariables <- return $ getVariables [pattern]
  expressionVariables <- getBoundVariables [] expression
  Foldable.foldlM
    (\_ targetName -> Foldable.foldlM
      (\_ sourceName -> ccPutCall
        (makeShapeEdge targetSignature targetName sourceName UnknownChange))
      ()
      expressionVariables)
    ()
    patternVariables

{- EVariable now can't be "_", and all are []. -}

getBoundVariables :: [Name] -> Expression -> State ClauseContext [Name]
getBoundVariables names (ENil name) = return names
getBoundVariables names (EVariable name []) = do
  isVariable <- isBoundVariable name
  if isVariable
  then return $ (name : names)
  else return names
getBoundVariables names (ENode name e1 e2) = do
  l1 <- getBoundVariables names e1
  getBoundVariables l1 e2
