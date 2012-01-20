module Syntax where

type Name = String

data Expression
  = ENode Expression Expression
  | EVariable Name [Expression]
  | ENil

instance Show Expression where
  show ENil = "0"
  show (EVariable name arguments) = ('(' : name) ++ (' ' : (show arguments))  ++ ")"
  show (ENode left right) = ('(' : (show left)) ++ ('.' : (show right)) ++ ")"

data Pattern
  = PNode Pattern Pattern
  | PVariable Name
  | PNil

instance Show Pattern where
  show PNil = "0"
  show (PVariable name) = name
  show (PNode left right) = ('(' : (show left)) ++ ('.' : (show right)) ++ ")"

data Clause
  = Clause Name [Pattern] Expression
  deriving(Show)

data Program
  = Program [Clause] Expression
  deriving(Show)
