module Syntax where

type Name = String

data Expression
  = ENode Expression Expression
  | EVariable Name [Expression]
  | ENil
  deriving(Show)

data Pattern
  = PNode Pattern Pattern
  | PVariable Name
  | PNil
  deriving(Show)

data Clause
  = Clause Name [Pattern] Expression
  deriving(Show)

data Program
  = Program [Clause] Expression
  deriving(Show)
