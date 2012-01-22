module Parser (parseFile, parseString) where

import Grammar
import Util

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Char
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
  pattern <- chainr1 parsePElement (parseCons PNode)
  spaces
  return pattern

parsePElement :: Parser Pattern
parsePElement = do
  parseNil PNil
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
exhaustiveFunction name functionClauses @ (clause : tailClauses) =
  let
    patterns = fClausePatterns clause

    initialSiblings = map (getSiblings) patterns
    finalSiblings = foldl
      (\siblings functionClause -> map
        (\(pattern, siblings) -> matchSiblings pattern siblings)
        (zip (fClausePatterns functionClause) siblings))
      initialSiblings
      tailClauses
  in
    if all (\siblingList -> length siblingList == 0) finalSiblings
    then functionClauses
    else error $ name ++ " is not exhaustive, wouldn't match " ++ (show finalSiblings)

matchSiblings :: Pattern -> [Pattern] -> [Pattern]
matchSiblings matchingPattern possiblePatterns =
  foldl (\acc pattern ->
    if matches matchingPattern pattern
    then acc
    else pattern : acc) [] possiblePatterns

matches :: Pattern -> Pattern -> Bool
matches PNil PNil = True
matches PNil _ = False
matches (PVariable _) _ = True
matches _ (PVariable _) = False
matches (PNode _ _) PNil = False
matches (PNode p11 p12) (PNode p21 p22) = matches p11 p21 && matches p12 p22

getSiblings :: Pattern -> [Pattern]
getSiblings PNil = [PNode (PVariable "_") (PVariable "_")]
getSiblings (PVariable _) = []
getSiblings (PNode leftP rightP) =
  let
    leftS = getSiblings leftP
    rightS = getSiblings rightP
    leftInit = map (\s -> (PNode leftP s)) rightS
    rightInit = map (\s -> (PNode s rightP)) leftS
  in
    [PNil] ++ leftInit ++ rightInit ++ mergeSiblings leftS rightS rightS

mergeSiblings :: [Pattern] -> [Pattern] -> [Pattern] -> [Pattern]
mergeSiblings [] _ _ = []
mergeSiblings leftS @ (leftH : leftT) (rightH : rightT) rightS =
  (PNode leftH rightH) : (mergeSiblings leftS rightT rightS)
mergeSiblings (_ : leftT) [] rightS =
  mergeSiblings leftT rightS rightS

