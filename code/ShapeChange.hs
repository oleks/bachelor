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

ccGet :: (ClauseContext -> t) -> State ClauseContext t
ccGet f = do
  context <- get
  return $ f context

ccGetVariablePatterns :: State ClauseContext VariablePatterns
ccGetVariablePatterns = ccGet ccVariablePatterns

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

{-

getContextVariables :: State ShapeContext Variables
getContextVariables = getFromState contextVariables

getCalleeName :: State ShapeContext Name
getCalleeName = getFromState calleeName

getCallerName :: State ShapeContext (Maybe Name)
getCallerName = getFromState callerName

isBoundVariable :: Name -> State ShapeContext Bool
isBoundVariable name = do
  variables <- getContextVariables
  return $ Map.member name variables

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

bindVariable :: Name -> Pattern -> State ShapeContext ()
bindVariable name pattern = do
  state <- get
  put $ state {
    contextVariables = (Map.insert name pattern (contextVariables state))
  }

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

{- end Shape Context Monad -}

-}

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
      in  analyzeClause clauseSignature functionClause)
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
  Map.insert name (PNil "_") variablePatterns
getVariablePatterns variablePatterns (PVariable name) =
  Map.insert name (PNil "_") variablePatterns
getVariablePatterns variablePatterns (PNode "_" p1 p2) =
  let
    vp1 = getVariablePatterns variablePatterns p1
    vp2 = getVariablePatterns variablePatterns p2
  in
    vp2
getVariablePatterns variablePatterns (PNode name p1 p2) =
  Map.insert name (PNode "_" p1 p2) variablePatterns

{- Note: There's supposedly no need to delve deaper in the last clause. -}

analyzeExpression :: Expression -> State ClauseContext ()
analyzeExpression ENil = return ()
analyzeExpression (ENode e1 e2) = do
  analyzeExpression e1
  analyzeExpression e2
  return ()
analyzeExpression (EVariable name []) = do
  isVariable <- isBoundVariable name
  if isVariable
  then return ()
  else analyzeCall (getSignature name []) []
analyzeExpression (EVariable name arguments) =
  analyzeCall (getSignature name arguments) arguments

analyzeCall :: FunctionSignature -> Arguments -> State ClauseContext ()
analyzeCall functionSignature arguments = do
  clauses <- ccGetClauses functionSignature
  return ()


purifyExpression :: Expression -> State ClauseContext Expression
purifyExpression ENil = return ENil
purifyExpression (ENode e1 e2) = do
  d1 <- purifyExpression e1
  d2 <- purifyExpression e2
  return $ liftM (liftM $ ENode $ purifyExpression e1) d2

{-
purifyExpression (EVariable name []) = do
  variables <- getContextVariables
  if Map.member name variables
  then return $ patternToExpression (variables Map.! name)
  else return $ EVariable "_" []
-}


{-
  functions <- getContextFunctions
  signature <- return $ getSignature name arguments
  if Map.member signature functions
  then do
    arguments <- mapM (deduceArgument) arguments
    clauses <- return $ functions Map.! signature
    setCalleeName signature
    foldlMWithIndex (matchClause arguments) () clauses
  else return ()
-}


{-


{-
analyzeClause
  bind variables
  analyze expression
-}

analyzeClause :: FunctionClause -> State ShapeContext ()
analyzeClause clause =
  let
    patterns = fClausePatterns clause
    expression = fClauseExpression clause
  in do
    setVariables Map.empty
    mapM (bindPatternVariables) patterns
    analyzeExpression expression

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
  hasVariable <- isBoundVariable name
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
  expressionVariables <- getBoundVariables [] expression
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

getBoundVariables :: [Name] -> Expression -> State ShapeContext [Name]
getBoundVariables names ENil = return names
getBoundVariables names (EVariable name []) = do
  hasVariable <- isBoundVariable name
  if hasVariable
  then return $ (name : names)
  else return names
getBoundVariables names (ENode e1 e2) = do
  l1 <- getBoundVariables names e1
  getBoundVariables l1 e2


-}
