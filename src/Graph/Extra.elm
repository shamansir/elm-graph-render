module Graph.Extra exposing (..)


import Graph exposing (Graph)


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
