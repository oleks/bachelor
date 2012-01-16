module Parser where

import Syntax

import Data.Char
import Text.ParserCombinators.Parsec

lowerAlpha = satisfy (\letter -> isLower letter && isAlpha letter) <?> "valid name"

charToken :: Char -> Parser ()
charToken letter = do
  char letter
  spaces
  return ()

stringToken :: String -> Parser ()
stringToken text = do
  string text
  spaces
  return ()

parseName :: Parser Name
parseName = do
  firstLetter <- lowerAlpha
  tailLetters <- many (lowerAlpha <|> char '-')
  spaces
  return $ [firstLetter] ++ tailLetters

parseEVariable :: Parser Expression
parseEVariable = do
  name <- parseName
  expressions <- many parseExpression
  return $ EVariable name expressions

parsePVariable :: Parser Pattern
parsePVariable = do
  name <- parseName
  return $ PVariable name

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

parseEElement :: Parser Expression
parseEElement = do
  parseNil ENil
  <|> parseEVariable
  <|> parseBrackets parseExpression

parseExpression :: Parser Expression
parseExpression = do
  expression <- chainr1 parseEElement (parseCons ENode)
  spaces
  return expression

parsePElement :: Parser Pattern
parsePElement = do
  parseNil PNil
  <|> parsePVariable
  <|> parseBrackets parsePattern

parsePattern :: Parser Pattern
parsePattern = do
  pattern <- chainr1 parsePElement (parseCons PNode)
  spaces
  return pattern

parseCons :: (t -> t-> t) -> Parser (t -> t -> t)
parseCons f = do
  charToken '.'
  return $ f

parseClause :: Parser Clause
parseClause = do
  name <- parseName <?> "clause name"
  patterns <- many1 parsePattern
  stringToken ":="
  expression <- parseExpression <?> "expression"
  return $ Clause name patterns expression

{- TODO: Change <program> to <clause>+<expression>, most interesting programs
have functions -}

parseProgram :: Parser Program
parseProgram = do
  clauses <- many1 parseClause
  expression <- parseExpression
  eof
  return $ Program clauses expression

parseString :: String -> Either ParseError Program
parseString programText = parse parseProgram ".." programText

