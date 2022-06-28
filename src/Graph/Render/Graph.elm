module Graph.Render.Graph exposing (..)

import Graph exposing (Graph)
import Graph.Extra as G
import Graph.Tree as Tree
import Svg exposing (Svg)
import Svg.Attributes as Svg
import Html exposing (Html)
import IntDict as ID exposing (IntDict)

import Graph.Tree.Geometry as Geom
import Graph.Render.Forest as Render


type alias NodesPositions = IntDict Geom.Position -- Dict Graph.NodeId Geom.Position


type Gap = Gap Float


graph
    :  Render.Defs msg
    -> Gap
    -> (Geom.Position -> NodesPositions -> Graph.NodeContext n e -> Html msg) --
    -> (n -> { width : Float, height : Float })
    -> Graph n e
    -> Html msg
graph defs_ gap renderNode sizeOfNode g =
    let
        forest = Graph.dfsForest (G.noParentsNodes g) g
        forestGeom = Geom.add (.node >> .label >> sizeOfNode) forest
        positions =
            forestGeom
                |> Geom.fold (\pos ctx list -> ( ctx.node.id, pos ) :: list) []
                |> List.concat
                |> ID.fromList
    in
    Render.forestGeometry
        defs_
        (\pos -> renderNode pos positions)
        forestGeom

    {-
    Render.forest
        renderNode
        (.node >> .label >> sizeOfNode)
        <| Graph.dfsForest (G.noParentsNodes g) g
    -}


graph_
    :  Render.Defs msg
    -> Gap
    -> (Geom.Position -> Graph.NodeContext n e -> Html msg)
    -> (n -> { width : Float, height : Float })
    -> Graph n e
    -> Html msg
graph_ defs_ gap renderNode sizeOfNode g =
    let
        nodes = g |> Graph.nodes |> List.map .label
    in
        g

            {- addition order -}
            {--}
            |> Graph.fold (::) []
            {--}
            {- /addition order -}

            {- DFS -}
            {-
            |> Graph.dfs (Graph.onDiscovery (::)) [] -- onFinish?
            |> List.reverse
            |> List.map (.node >> .label)
            -}
            {- /DFS: onDiscovery -}

            {- DFS Tree -}
            {-
            |> Graph.dfsTree 1 -- onFinish?
            |> Tree.levelOrderList
            |> List.map (.node >> .label)
            -}
            {- /DFS: onDiscovery -}

            {- BFS -}
            {-
            |> Graph.bfs (Graph.ignorePath (::)) []
            |> List.reverse
            |> List.map (.node >> .label)
            -}
            {- /DFS: onDiscovery -}

            {- Guided DFS -}
            {-
            |> Graph.guidedDfs Graph.alongOutgoingEdges (Graph.onDiscovery (::)) [ 4 ] []
            |> Tuple.first
            |> List.map (.node >> .label)
            -}
            {- /Guided DFS -}

            {- Guided BFS -}
            {-
            |> Graph.guidedBfs Graph.alongOutgoingEdges (Graph.ignorePath (::)) [ 0 ] []
            |> Tuple.first
            |> List.map (.node >> .label)
            -}
            {- /Guided BFS -}

            {- Topological -}
            {-
            |> Graph.guidedBfs Graph.alongOutgoingEdges (Graph.ignorePath (::)) [ 0 ] []
            |> Tuple.first
            |> List.map (.node >> .label)
            -}
            {- /Topological -}

            {--}
            |> distributeByHeight gap (.node >> .label >> sizeOfNode >> .height)
            |> List.map (\(y, ctx) -> renderNode { x = 0, y = y } ctx)
            {--}

            |> ((::) (Svg.defs [] <| Render.unDefs defs_))
            |> Svg.svg
                [ Svg.width "1000px"
                , Svg.height <| String.fromFloat (totalHeight gap (sizeOfNode >> .height) nodes) ++ "px"
                ]

-- distributeByHeight : Float -> (a -> Float) -> List a -> List (Float, a)
-- distributeByHeight gap toHeight =


totalHeight : Gap -> (a -> Float) -> List a -> Float
totalHeight (Gap gap) fullHeight items =
    ( items
        |> List.map fullHeight
        |> List.sum
    ) + gap * toFloat (List.length items - 1)


distributeByHeight : Gap -> (a -> Float) -> List a -> List (Float, a)
distributeByHeight (Gap gap) toHeight =
    List.foldl
        (\a ( prevHeight, vals ) ->
            ( prevHeight + gap + toHeight a
            , (prevHeight, a)
                :: vals
            )
        ) ( 0, [] )
        >> Tuple.second