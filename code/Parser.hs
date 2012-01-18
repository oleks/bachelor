module Parser (parseFile, parseString) where

import Syntax

import Data.Char
import Text.ParserCombinators.Parsec

{- begin Parsing -}

parseFile :: String -> IO (Either ParseError Program)
parseFile fileName = parseFromFile parseProgram fileName

parseString :: String -> Either ParseError Program
parseString programText = parse parseProgram ".." programText

parseProgram :: Parser Program
parseProgram = do
  clauses <- many1 parseClause
  expression <- parseExpression
  eof
  return $ Program clauses expression

{- end Parsing -}

{- begin Clauses -}

parseClause :: Parser Clause
parseClause = do
  (name, patterns) <- try parseClauseDeclaration
  expression <- parseExpression <?> "expression"
  return $ Clause name patterns expression

parseClauseDeclaration :: Parser (Name, [Pattern])
parseClauseDeclaration = do
  name <- parseName <?> "clause name"
  patterns <- many1 parsePattern
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

{- TODO: Change <program> to <clause>+<expression>, most interesting programs
have functions -}
