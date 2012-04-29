module Grammar where

import qualified Data.Map as Map

type Name = String

type Arguments = [Expression]

data Expression
  = ENil
  | EVariable Name Arguments
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
    (\name clauseList formerFunction -> ('\n' : name) ++ "\n" ++ foldlWithIndex
      (\index formerClauses clause -> (show index) ++ ":  " ++ (show clause) ++
        ('\n' : formerClauses))
      formerFunction
      clauseList)
    (show $ fpExpression functionProgram)
    (fpFunctions functionProgram)

type FunctionSignature = Name
type ClauseIndex = Int
type ClauseSignature = Name
type VariableName = Name

getClauseSignature :: FunctionSignature -> ClauseIndex -> ClauseSignature
getClauseSignature functionSignature index = functionSignature ++ ('/' : (show index))

data ShapeEdge = ShapeEdge ClauseSignature ClauseSignature Name Name ShapeChange
  deriving (Show)
type ShapeGraph = [ShapeEdge]

data ShapeChange
  = Less
  | LessOrEqual
  | UnknownChange
  deriving(Eq,Show)

foldlWithIndex :: (Int -> a -> b -> a) -> a -> [b] -> a
foldlWithIndex function initial list =
  let
    init = ((length list) - 1, initial)
    (_, a) = foldl (\(index,a) b -> (index - 1, function index a b)) init list
  in
    a

foldrWithIndex :: (Int -> a -> b -> a) -> a -> [b] -> a
foldrWithIndex function initial list =
  let
    init = ((length list) - 1, initial)
    (_, a) = foldr (\b (index,a) -> (index - 1, function index a b)) init list
  in
    a

