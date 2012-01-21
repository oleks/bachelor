module Parser (parseFile, parseString) where

import Syntax
import Util

import qualified Data.Map as Map

import Data.Char
import Text.ParserCombinators.Parsec

{- begin Parsing -}

parseFile :: String -> IO (Either ParseError ValidProgram)
parseFile fileName = parseFromFile parseProgram fileName

parseString :: String -> Either ParseError ValidProgram
parseString programText = parse parseProgram ".." programText

parseProgram :: Parser ValidProgram
parseProgram = do
  clauses <- many1 parseClause
  expression <- parseExpression
  eof
  (frame, functions) <- return $ initializeFunctions clauses [] Map.empty
  normalExpression <- return $ normalizeExpression [] frame expression
  return $ ValidProgram frame functions normalExpression

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
  name <- parseName
  return $ PVariable name

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
  <|> parseEVariable
  <|> parseBrackets parseExpression

parseEVariable :: Parser Expression
parseEVariable = do
  name <- parseName
  expressions <- many parseExpression
  return $ EVariable name expressions

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

initializeFunctions :: [Clause] -> Frame -> Functions -> (Frame, Functions)
initializeFunctions [] frame functions = (frame, ensureExhaustive functions)
initializeFunctions ((Clause name patterns expression) : tail) frame functions =
  let
    signature = getUnarySignature name
    signatureExists = Map.member signature functions
    functionFrame =
      if signatureExists
      then frame
      else signature : frame
    staticClause = normalizeClause patterns expression functionFrame
    functionClauses = staticClause :
      if signatureExists
      then functions Map.! signature
      else []
  in
    initializeFunctions tail functionFrame (Map.insert signature functionClauses functions)

normalizeClause :: [Pattern] -> Expression -> Frame -> StaticClause
normalizeClause patterns expression functionFrame =
  let
    (names, normalPattern) = foldl (\(names, acc) pattern ->
      (getVariableNames pattern names, PNode acc pattern)) ([], PNil) patterns
    normalExpression = normalizeExpression names functionFrame expression
  in
    StaticClause [normalPattern] normalExpression functionFrame

normalizeExpression :: [Name] -> Frame -> Expression -> Expression
normalizeExpression variableNames functionFrame ENil = ENil
normalizeExpression variableNames functionFrame (ENode leftE rightE) =
  let
    normalLeftE = normalizeExpression variableNames functionFrame leftE
    normalRightE = normalizeExpression variableNames functionFrame rightE
  in
    ENode normalLeftE normalRightE
normalizeExpression variableNames functionFrame (EVariable name arguments) =
  let
    signature = getUnarySignature name
    argument = foldl (\expression acc -> (ENode expression acc)) ENil arguments
  in
    if any (== name) variableNames
    then
      if length arguments == 0
      then EVariable name []
      else error $ name ++ " is a variable and therefore takes no arguments"
    else
      if any (== signature) functionFrame
      then EVariable name [argument]
      else error $ signature ++ " does not exist"

ensureExhaustive :: Functions -> Functions
ensureExhaustive functions = Map.mapWithKey ensureExhaustiveFunction functions

ensureExhaustiveFunction :: Name -> [StaticClause] -> [StaticClause]
ensureExhaustiveFunction name clauses = clauses

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


