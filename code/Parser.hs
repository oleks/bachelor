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
  return $ ValidProgram frame functions expression

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
    signature = getSignature name patterns
    signatureExists = Map.member signature functions
    functionFrame =
      if signatureExists
      then frame
      else signature : frame
    staticClause = StaticClause patterns expression functionFrame
    functionClauses = staticClause :
      if signatureExists
      then functions Map.! signature
      else []
  in
    initializeFunctions tail functionFrame (Map.insert signature functionClauses functions)

ensureExhaustive :: Functions -> Functions
ensureExhaustive functions = Map.mapWithKey ensureExhaustiveFunction functions

ensureExhaustiveFunction :: Name -> [StaticClause] -> [StaticClause]
ensureExhaustiveFunction name clauses = clauses

getSiblings :: Pattern -> [Pattern]
getSiblings PNil = [PNode (PVariable "_") (PVariable "_")]
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


