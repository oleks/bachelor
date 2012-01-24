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
  = PNode Name Pattern Pattern
  | PVariable Name
  | PNil Name

instance Show Pattern where
  show (PNil "_") = "0"
  show (PNil name) = name ++ "@0"
  show (PVariable name) = name
  show (PNode name left right) =
    let
      tail = ('(' : (show left)) ++ ('.' : (show right)) ++ ")"
    in
      if name == "_" then tail else name ++ ('@' : tail)

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

type Functions = Map.Map Name [FunctionClause]

data FunctionProgram
  = FunctionProgram {
    fpFunctions :: Functions,
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

type ClauseSignature = Name

getClauseSignature :: Name -> Int -> ClauseSignature
getClauseSignature functionSignature index = functionSignature ++ ('/' : (show index))

data SCEdge = SCEdge ClauseSignature ClauseSignature Name Name Change
  deriving (Show)
type SCGraph = [SCEdge]

data Change
  = LT
  | LEQ
  | UNKNOWN
  deriving(Eq,Show)
