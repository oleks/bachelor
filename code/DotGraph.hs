module DotGraph (getDotGraph) where

import Grammar
import qualified Data.Char as Char

data DotGraph
  = DotGraph {
    dotName :: String,
    dotNodes :: String,
    dotEdges :: String
  }

dotGraph :: String -> String -> String -> DotGraph
dotGraph name nodes edges =
  DotGraph {
    dotName = name,
    dotNodes = nodes,
    dotEdges = edges
  }

getNewName :: String -> String
getNewName name
  = case name of
    'z' : tail -> 'a' : 'z' : tail
    head : tail -> (Char.chr ((Char.ord head) + 1) ) : tail

getDotGraph :: Expression -> String
getDotGraph expression =
  let
    graph = getDotGraphAux "a" expression
    nodes = dotNodes graph
    edges = dotEdges graph
  in
    "digraph G {\n" ++ nodes ++ edges ++ "}"

getDotGraphAux :: String -> Expression -> DotGraph
getDotGraphAux initialName ENil =
  let
    nodes = initialName ++ "[shape=circle,fillcolor=white,label=\"\"];\n"
    edges = []
  in
    dotGraph initialName nodes edges
getDotGraphAux initialName (ENode e1 e2) =
  let
    name1 = getNewName initialName
    graph1 = getDotGraphAux name1 e1
    name2 = getNewName (dotName graph1)
    graph2 = getDotGraphAux name2 e2
    nodes = initialName ++ "[shape=circle,fillcolor=black,style=filled,label=\"\"];\n" ++
      (dotNodes graph1) ++ (dotNodes graph2)
    edges = initialName ++ "->{" ++ name1 ++ (';' : name2) ++
      ('}' : ';' : '\n' : (dotEdges graph1)) ++ (dotEdges graph2)
  in
    dotGraph (dotName graph2) nodes edges
getDotGraphAux initialName _ =
  let
    nodes = initialName ++ "[shape=triangle,fillcolor=white,label=\"\"];\n"
    edges = []
  in
    dotGraph initialName nodes edges

