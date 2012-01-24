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

analyzeFileToDot :: String -> IO String
analyzeFileToDot fileName = do
  shapeChangeGraph <- analyzeFile fileName
  return $ getSCGraph shapeChangeGraph

analyzeFile :: String -> IO SCGraph
analyzeFile fileName = do
  ast <- Parser.parseFile fileName
  analyze ast

analyze :: (Either Parsec.ParseError FunctionProgram) -> IO SCGraph
analyze ast =
  case ast of
    Left errorText -> error $ show errorText
    Right program -> analyzeProgram program

type Variables = Map.Map Name Pattern

data ShapeContext
  = ShapeContext {
    callerName :: Maybe Name,
    calleeName :: Name,
    contextFunctions :: Functions,
    contextVariables :: Variables,
    shapeGraph :: SCGraph
  }

getFromState :: (ShapeContext -> t) -> State ShapeContext t
getFromState f = do
  state <- get
  return $ f state

getContextFunctions :: State ShapeContext Functions
getContextFunctions = getFromState contextFunctions

getContextVariables :: State ShapeContext Variables
getContextVariables = getFromState contextVariables

hasContextVariable :: Name -> State ShapeContext Bool
hasContextVariable name = do
  variables <- getContextVariables
  return $ Map.member name variables

getCalleeName :: State ShapeContext Name
getCalleeName = getFromState calleeName

getCallerName :: State ShapeContext (Maybe Name)
getCallerName = getFromState callerName

setCalleeName :: Name -> State ShapeContext ()
setCalleeName name = do
  state <- get
  put $ state { calleeName = name }

setCallerName :: Name -> State ShapeContext ()
setCallerName name = do
  state <- get
  put $ state { callerName = (Just name) }

setVariables :: Variables -> State ShapeContext ()
setVariables variables = do
  state <- get
  put $ state { contextVariables = variables }

setVariable :: Name -> Pattern -> State ShapeContext ()
setVariable name pattern = do
  state <- get
  put $ state { contextVariables = (Map.insert name pattern (contextVariables state)) }

putCall :: SCEdge -> State ShapeContext ()
putCall edge = do
  state <- get
  put $ state { shapeGraph = edge : (shapeGraph state) }

putSourceTargetShape :: Name -> Change -> Name -> State ShapeContext ()
putSourceTargetShape sourceName shape targetName = do
  callerName <- getCallerName
  calleeName <- getCalleeName
  case callerName of
    Just name -> putCall $ SCEdge name calleeName sourceName targetName shape
    _ -> return ()

putShape :: Name -> Name -> Change -> State ShapeContext ()
putShape sourceName targetName shape = putSourceTargetShape sourceName shape targetName

putTargetSourceShape :: Name -> Change -> Name -> State ShapeContext ()
putTargetSourceShape targetName shape sourceName = putSourceTargetShape sourceName shape targetName

analyzeProgram :: FunctionProgram -> IO SCGraph
analyzeProgram functionProgram =
  let
    functions = Map.map (reverse) (fpFunctions functionProgram)
    initialContext = ShapeContext Nothing "" functions Map.empty []
    finalContext = execState (analyzeFunctions functions) initialContext
  in
    return (shapeGraph finalContext)

analyzeFunctions :: Functions -> State ShapeContext ()
analyzeFunctions functions = Foldable.foldlM
  (\_ (signature, clauses) -> foldlMWithIndex
    (\index _ clause -> do
      setCallerName (getClauseSignature signature index)
      analyzeClause clause)
    ()
    clauses)
  ()
  (Map.toList functions)

analyzeExpression :: Expression -> State ShapeContext ()
analyzeExpression ENil = return ()
analyzeExpression (ENode e1 e2) = do
  analyzeExpression e1
  analyzeExpression e2
  return ()
analyzeExpression (EVariable name arguments) = do
  functions <- getContextFunctions
  signature <- return $ getSignature name arguments
  if Map.member signature functions
  then do
    arguments <- mapM (deduceArgument) arguments
    clauses <- return $ functions Map.! signature
    setCalleeName signature
    foldlMWithIndex (matchClause arguments) () clauses
  else return ()

deduceArgument :: Expression -> State ShapeContext Expression
deduceArgument ENil = return ENil
deduceArgument (ENode e1 e2) = do
  d1 <- deduceArgument e1
  d2 <- deduceArgument e2
  return $ ENode d1 d2
deduceArgument (EVariable name []) = do
  variables <- getContextVariables
  if Map.member name variables
  then return $ patternToExpression (variables Map.! name)
  else return $ EVariable "_" []

patternToExpression :: Pattern -> Expression
patternToExpression (PNil _) = ENil
patternToExpression (PVariable _) = EVariable "_" []
patternToExpression (PNode _ p1 p2) =
  let
    e1 = patternToExpression p1
    e2 = patternToExpression p2
  in
    (ENode e1 e2)

matchClause :: [Expression] -> Int -> () -> FunctionClause -> State ShapeContext ()
matchClause arguments clauseIndex _ clause =
  let
    patterns = fClausePatterns clause
    allMatch = all (\(pattern, shape) -> matchesShape pattern shape) (zip patterns arguments)
  in
    if allMatch
    then deduceRelations arguments patterns clauseIndex
    else return ()

deduceRelations :: [Expression] -> [Pattern] -> Int -> State ShapeContext ()
deduceRelations arguments patterns clauseIndex = do
  callerName <- getCallerName
  calleeName <- getCalleeName
  pureArguments <- mapM (purifyExpression) arguments
  case callerName of
    Nothing -> return ()
    Just name -> do
      setCalleeName (getClauseSignature calleeName clauseIndex)
      mapM (deduceRelationsAux) (zip pureArguments patterns)
      return ()

purifyExpression :: Expression -> State ShapeContext Expression
purifyExpression ENil = return ENil
purifyExpression (EVariable name []) = do
  hasVariable <- hasContextVariable name
  if hasVariable
  then return $ EVariable name []
  else return $ EVariable "_" []
purifyExpression (ENode e1 e2) = do
  p1 <- purifyExpression e1
  p2 <- purifyExpression e2
  return $ ENode e1 e2

deduceRelationsAux :: (Expression, Pattern) -> State ShapeContext ()

deduceRelationsAux (ENil, _) = return ()
deduceRelationsAux (_,(PNil _)) = return ()

deduceRelationsAux (EVariable "_" _, _) = return ()
deduceRelationsAux (_,(PVariable "_")) = return ()

deduceRelationsAux (EVariable sourceName _, PVariable targetName) =
  putShape sourceName targetName LEQ
deduceRelationsAux (expression, PVariable targetName) = do
  expressionVariables <- getExpressionVariables [] expression
  setAll (putTargetSourceShape targetName UNKNOWN) expressionVariables

deduceRelationsAux (EVariable sourceName [], pattern @ (PNode "_" p1 p2)) =
  let
    patternVariables = getVariables [pattern]
  in
    setAll (putSourceTargetShape sourceName Grammar.LT) patternVariables
deduceRelationsAux (EVariable sourceName _, PNode targetName p1 p2) =
  let
    variables = getVariables [p1,p2]
  in do
    putShape sourceName targetName LEQ
    setAll (putSourceTargetShape sourceName Grammar.LT) variables

deduceRelationsAux ((ENode e1 e2),(PNode _ p1 p2)) = do
  deduceRelationsAux (e1,p1)
  deduceRelationsAux (e2,p2)

setAll :: (Name -> State ShapeContext ()) -> [Name] -> State ShapeContext ()
setAll f names = Foldable.foldlM (\_ name -> f name) () names

getExpressionVariables :: [Name] -> Expression -> State ShapeContext [Name]
getExpressionVariables names ENil = return names
getExpressionVariables names (EVariable name []) = do
  hasVariable <- hasContextVariable name
  if hasVariable
  then return $ (name : names)
  else return names
getExpressionVariables names (ENode e1 e2) = do
  l1 <- getExpressionVariables names e1
  getExpressionVariables l1 e2


analyzeClause :: FunctionClause -> State ShapeContext ()
analyzeClause clause =
  let
    patterns = fClausePatterns clause
    expression = fClauseExpression clause
  in do
    setVariables Map.empty
    mapM (mapVariables) patterns
    analyzeExpression expression
    setVariables Map.empty

mapVariables :: Pattern -> State ShapeContext ()
mapVariables (PNil "_") = return ()
mapVariables (PNil name) = setVariable name (PNil "_")
mapVariables (PVariable "_") = return ()
mapVariables (PVariable name) = setVariable name (PNil "_")
mapVariables (PNode "_" p1 p2) = do
  mapVariables p1
  mapVariables p2
mapVariables (PNode name p1 p2) = setVariable name (PNode "_" p1 p2)


{-
analyzeFunctions :: Functions -> IO ShapeChangeGraph
analyzeFunctions functions = return $ Map.foldrWithKey
  (\functionSignature clauses graph -> foldlWithIndex
    (\clauseIndex graph clause ->
      let
        clauseSignature = getClauseSignature functionSignature clauseIndex
        expression = fClauseExpression clause
      in
        getCallsInExpression functions clauseSignature graph expression)
    graph
    clauses)
  []
  functions

getCallsInExpression :: Functions -> ClauseSignature -> ShapeChangeGraph -> Expression -> ShapeChangeGraph
getCallsInExpression functions clauseSignature graph ENil = graph
getCallsInExpression functions clauseSignature graph (EVariable name arguments) =
  let
    calleeSignature = getSignature name arguments
  in
    if Map.member calleeSignature functions
    then
      let
        matchingSignatures = getMatchingSignatures (functions Map.! calleeSignature) arguments
        calls = map
          (\calleeIndex -> (clauseSignature, (getClauseSignature calleeSignature calleeIndex), arguments))
          matchingSignatures
      in
        calls ++ graph
    else graph
getCallsInExpression functions clauseSignature graph (ENode e1 e2) =
  let
    g1 = getCallsInExpression functions clauseSignature graph e1
  in
    getCallsInExpression functions clauseSignature g1 e2

getMatchingSignatures :: [FunctionClause] -> [Expression] -> [Int]
getMatchingSignatures clauses arguments = foldlWithIndex
  (\clauseIndex indices clause ->
    let
      patterns = fClausePatterns clause
    in
      if (all (\(pattern, shape) -> matchesShape pattern shape) (zip patterns arguments))
      then (clauseIndex : indices)
      else indices)
  []
  clauses

-}
