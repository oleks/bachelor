module Grammar where

import qualified Data.Map as Map

type Name = String

data Expression
  = ENil
  | EVariable Name [Expression]
  | ENode Expression Expression

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
  = Clause {
    clauseName :: Name,
    clausePatterns :: [Pattern],
    clauseExpression :: Expression
  }
instance Show Clause where
  show clause = (clauseName clause) ++ " " ++ (show $ clausePatterns clause)
    ++ " := " ++ (show $ clauseExpression clause) ++ ";"

data ClauseProgram
  = ClauseProgram {
    cpClauses :: [Clause],
    cpExpression :: Expression
  }
instance Show ClauseProgram where
  show clauseProgram = foldl
    (\partial clause -> (show clause) ++ ('\n' : partial))
    (show $ cpExpression clauseProgram)
    (cpClauses clauseProgram)

type Frame = [Name]
emptyFrame = []

type Frames = Map.Map Name [Name]

data FunctionClause
  = FunctionClause {
    fClausePatterns :: [Pattern],
    fClauseExpression :: Expression,
    fClauseFrame :: Frame
  }
instance Show FunctionClause where
  show clause = (show $ fClausePatterns clause)
    ++ " := " ++ (show $ fClauseExpression clause)
    ++ " / " ++ (show $ fClauseFrame clause)


data FunctionProgram
  = FunctionProgram {
    fpFunctions :: Map.Map Name [FunctionClause],
    fpExpression :: Expression
  }
emptyFunctionProgram :: Expression -> FunctionProgram
emptyFunctionProgram = FunctionProgram Map.empty

instance Show FunctionProgram where
  show functionProgram = Map.foldrWithKey
    (\name clauseList formerFunction -> ('\n' : name) ++ "\n" ++ foldr
      (\clause formerClauses -> "  " ++ (show clause) ++ ('\n' : formerClauses))
      formerFunction
      clauseList)
    (show $ fpExpression functionProgram)
    (fpFunctions functionProgram)
