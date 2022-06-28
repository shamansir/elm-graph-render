module Graph.Extra exposing (..)


import Graph exposing (Graph)
import Dict


filterMap : (n1 -> Maybe n2) -> Graph n1 e -> Graph n2 e
filterMap filterFn graph =
    let
        nodes = Graph.nodes graph
        edges = Graph.edges graph
    in
    Graph.fromNodesAndEdges
        (nodes |> List.filterMap
            (\n1 ->
                filterFn n1.label |> Maybe.map (\n2 -> { id = n1.id, label = n2 })
            )
        )
        edges


filter : (n -> Bool) -> Graph n e -> Graph n e
filter filterFn graph =
    let
        nodes = Graph.nodes graph
        edges = Graph.edges graph
    in
    Graph.fromNodesAndEdges
        (nodes |> List.filter (.label >> filterFn))
        edges


noParentsNodes : Graph n e -> List Graph.NodeId
noParentsNodes g =
    let
        oneOrAdd =
            Maybe.map ((+) 1) >> Maybe.withDefault 1 >> Just
        nodesThatHaveParents =
            Graph.edges g
                |> List.foldl (\{ to } dict -> Dict.update to oneOrAdd dict) Dict.empty
                |> Dict.keys
    in
        Graph.nodes g
            |> List.filter (\n -> not <| List.member n.id nodesThatHaveParents)
            |> List.map .id