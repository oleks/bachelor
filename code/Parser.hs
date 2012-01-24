module Parser (parseFile, parseString) where

import Grammar
import Util

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Char
import Data.List
import Text.ParserCombinators.Parsec

{- begin Parsing -}

parseFile :: String -> IO (Either ParseError FunctionProgram)
parseFile fileName = parseFromFile parseProgram fileName

parseString :: String -> Either ParseError FunctionProgram
parseString programText = parse parseProgram ".." programText

parseProgram :: Parser FunctionProgram
parseProgram = do
  clauses <- many1 parseClause
  expression <- parseExpression
  eof
  clauseProgram <- return $ ClauseProgram clauses expression
  frames <- return $ getFrames clauses
  return $ getFunctionProgram frames clauseProgram

{- end Parsing -}

{- begin Clauses -}

parseClause :: Parser Clause
parseClause = do
  (name, patterns) <- try parseClauseDeclaration
  expression <- parseExpression <?> "expression"
  charToken ';'
  return $ Clause name patterns expression

parseClauseDeclaration :: Parser (Name, [Pattern])
parseClauseDeclaration = do
  name <- parseName <?> "clause name"
  patterns <- many parsePattern
  stringToken ":="
  return (name, patterns)

stringToken :: String -> Parser ()
stringToken text = do
  string text
  spaces
  return ()

{- end Clauses -}

{- begin Patterns -}

parsePattern :: Parser Pattern
parsePattern = do
  pattern <- chainr1 parsePElement (parseCons (PNode "_"))
  spaces
  return pattern

parsePElement :: Parser Pattern
parsePElement = do
  parseNil (PNil "_")
  <|> parsePVariable
  <|> parseBrackets parsePattern

parsePVariable :: Parser Pattern
parsePVariable = do
  name <- parseName <|> parseUnderscore
  return $ PVariable name

parseUnderscore :: Parser Name
parseUnderscore = do
  stringToken "_"
  return $ "_"

{- end Patterns -}

{- begin Expressions -}

parseExpression :: Parser Expression
parseExpression = do
  expression <- chainr1 parseEElement (parseCons ENode)
  spaces
  return expression

parseEElement :: Parser Expression
parseEElement = do
  parseNil ENil
  <|> parseVariable
  <|> parseBrackets parseExpression

parseVariable :: Parser Expression
parseVariable = do
  name <- parseName
  expressions <- try parseSingletonExpression <|> return []
  return $ EVariable name expressions

parseSingletonExpression :: Parser [Expression]
parseSingletonExpression = do
  expression <- parseExpression
  return [expression]

{- end Expressions -}

{- begin Patterns & Expressions -}

parseCons :: (t -> t-> t) -> Parser (t -> t -> t)
parseCons f = do
  charToken '.'
  return $ f

parseNil :: t -> Parser t
parseNil t = do
  charToken '0'
  return t

parseBrackets :: Parser t -> Parser t
parseBrackets tParser = do
  charToken '('
  t <- tParser
  charToken ')'
  return $ t

{- end Patterns & Expressions -}

{- begin Auxiliary Functions -}

charToken :: Char -> Parser ()
charToken letter = do
  char letter
  spaces
  return ()

parseName :: Parser Name
parseName = do
  firstLetter <- lowerAlpha
  tailLetters <- many (lowerAlpha <|> char '-')
  spaces
  return $ [firstLetter] ++ tailLetters

lowerAlpha = satisfy (\letter -> isLower letter && isAlpha letter) <?> "valid name"

{- end Auxiliary Functions -}

{-**
@time O(n*log(n))
@where n = length clauses
-}
getFrames :: [Clause] -> Frames
getFrames clauses =
  let
    (_, map) = foldl
      (\(frame, map) clause ->
        let
          signature = getSignature (clauseName clause) (clausePatterns clause)
          newFrame = Set.insert signature frame
          newMap = Map.insert signature newFrame map
        in
          (newFrame, newMap))
      (Set.empty, Map.empty)
      clauses
    listMap = Map.map (\set -> Set.toList set) map
  in
    listMap

getFunctionProgram :: Frames -> ClauseProgram -> FunctionProgram
getFunctionProgram frames clauseProgram =
  let
    clauses = reverse $ cpClauses clauseProgram
    expression = cpExpression clauseProgram
    initialFunctionProgram = emptyFunctionProgram expression
    functionProgram = processClauses clauses frames initialFunctionProgram
    validProgram = validFunctionProgram functionProgram
    exhaustiveProgram = exhaustiveFunctionProgram validProgram
  in
    exhaustiveProgram

{-**
@time O(n*log(n))
@where n = length clauses
-}
processClauses :: [Clause] -> Frames -> FunctionProgram -> FunctionProgram
processClauses [] _ functionProgram = functionProgram
processClauses (clause : tailClauses) frames functionProgram =
  let
    name = clauseName clause
    patterns = clausePatterns clause
    expression = clauseExpression clause

    signature = getSignature name patterns
    functions = fpFunctions functionProgram
    signatureExists = Map.member signature functions

    functionFrame = frames Map.! signature
    functionClause = FunctionClause patterns expression functionFrame
    functionClauses =
      functionClause : if signatureExists then functions Map.! signature else []
    newFunctions = Map.insert signature functionClauses functions

    newFunctionProgram = functionProgram { fpFunctions = newFunctions }
  in
    processClauses tailClauses frames newFunctionProgram

{- begin Undefined Checking -}

{---
@time O(n * validClause)
@where n = Map.size functionProgram
-}
validFunctionProgram :: FunctionProgram -> FunctionProgram
validFunctionProgram functionProgram =
  let
    functions = fpFunctions functionProgram
    expression = fpExpression functionProgram
    newFunctions = Map.map (\clauses -> map validClause clauses) functions
    newExpression = validExpression [] (Map.keys functions) expression
  in
    FunctionProgram newFunctions newExpression

{---
@time O(n + validExpression)
@where n = length $ fClausePatterns functionClause
-}
validClause :: FunctionClause -> FunctionClause
validClause functionClause =
  let
    patterns = fClausePatterns functionClause
    expression = fClauseExpression functionClause
    frame = fClauseFrame functionClause

    variables = getVariables patterns
    newExpression = validExpression variables frame expression
  in
    FunctionClause patterns newExpression frame

{---
@time O(n*(m+l))
@where n = number of ENodes in the expression
@where m = length variableNames
@where l = length functionFrame
-}
validExpression :: [Name] -> Frame -> Expression -> Expression
validExpression variableNames functionFrame ENil = ENil
validExpression variableNames functionFrame (ENode leftE rightE) =
  let
    validLeftE = validExpression variableNames functionFrame leftE
    validRightE = validExpression variableNames functionFrame rightE
  in
    ENode validLeftE validRightE
validExpression variableNames functionFrame (EVariable name arguments) =
  let
    signature = getSignature name arguments
  in
    if (any (== name) variableNames) && (length arguments == 0)
    then EVariable name []
    else
      if any (== signature) functionFrame
      then EVariable name arguments
      else error $ signature ++ " does not exist"

{- end Undefined Checking -}

exhaustiveFunctionProgram :: FunctionProgram -> FunctionProgram
exhaustiveFunctionProgram functionProgram =
  let
    functions = fpFunctions functionProgram
    expression = fpExpression functionProgram
    newFunctions = Map.mapWithKey exhaustiveFunction functions
    newExpression = validExpression [] (Map.keys functions) expression
  in
    FunctionProgram newFunctions newExpression

exhaustiveFunction :: Name -> [FunctionClause] -> [FunctionClause]
exhaustiveFunction _ [] = []
exhaustiveFunction name (clause : tailClauses) =
  let
    patterns = fClausePatterns clause
    initialSiblings = map (getSiblings) patterns
    (finalSiblings, finalClauses) = foldl
      (\(siblings, clauses) clause ->
        let
          patterns = fClausePatterns clause
          (good, bad) = unzip $ map
            (\(pattern, siblings) -> matchClauseToSiblings pattern siblings)
            (zip patterns siblings)
          newClauses = synthesize (transpose good) clause clauses
          unnamedBad = map (map (unnameAll)) bad
        in
          (unnamedBad, newClauses))
      (initialSiblings, [clause])
      tailClauses
  in
    if all (\siblingList -> length siblingList == 0) finalSiblings
    then finalClauses
    else error $ name ++ " is not exhaustive, wouldn't match " ++ (show finalSiblings)

synthesize :: [[Pattern]] -> FunctionClause -> [FunctionClause] -> [FunctionClause]
synthesize [] _ clauses = clauses
synthesize (patterns : tailPatterns) clause clauses =
  synthesize tailPatterns clause (clause { fClausePatterns = patterns } : clauses)

unnameAll :: Pattern -> Pattern
unnameAll (PNil _) = PNil "_"
unnameAll (PVariable _) = PVariable "_"
unnameAll (PNode _ p1 p2) =
  let
    unnamedP1 = unnameAll p1
    unnamedP2 = unnameAll p2
  in
    PNode "_" unnamedP1 unnamedP2

{---
matchClauseToSiblings :: clausePattern -> activeSiblings -> (good, bad)
@where good are those shapes that will be matched by the pattern.
@where bad are those shapes that won't be matche by the pattern.
-}
matchClauseToSiblings :: Pattern -> [Pattern] -> ([Pattern], [Pattern])
matchClauseToSiblings clausePattern activeSiblings =
  let
    clausePatternSiblings = getSiblings clausePattern
  in
    foldl (\(good, bad) activeSibling ->
      if matches activeSibling clausePattern
      then (clausePattern : good, (filter (matches activeSibling) clausePatternSiblings) ++ bad)
      else if matches clausePattern activeSibling
        then ((mergeNames clausePattern activeSibling) : good, bad)
        else (good, activeSibling : bad))
    ([], [])
    activeSiblings

mergeNames :: Pattern -> Pattern -> Pattern
mergeNames (PNil name) (PNil _) = PNil name
mergeNames (PNil name) (PVariable _) = PVariable name
mergeNames (PNil name) (PNode _ p1 p2) = PNode name p1 p2
mergeNames (PVariable name) (PNil _) = PNil name
mergeNames (PVariable name) (PVariable _) = PVariable name
mergeNames (PVariable name) (PNode _ p1 p2) = PNode name p1 p2
mergeNames (PNode name _ _) (PNil _) = PNil name
mergeNames (PNode name _ _) (PVariable _) = PVariable name
mergeNames (PNode name n1 n2) (PNode _ p1 p2) =
  let
    m1 = mergeNames n1 p1
    m2 = mergeNames n2 p2
  in
    PNode name m1 m2

