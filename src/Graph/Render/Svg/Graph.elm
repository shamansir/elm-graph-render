module Graph.Render.Svg.Graph exposing (graph, graphWithDefs, NodesPositions)

{-|

# Rendering

@docs graph, graphWithDefs

# Nodes Positions

@docs NodesPositions
-}

import Graph exposing (Graph)
import Graph.Extra as G
import Graph.Tree as Tree
import Svg exposing (Svg)
import Svg.Attributes as Svg
import Html exposing (Html)
import IntDict as ID exposing (IntDict)

import Graph.Geometry as Geom
import Graph.Geometry.Make exposing (Way)
import Graph.Geometry.Make as Geom
import Graph.Render.Svg.Defs as D
import Graph.Render.Svg.Forest as Render


type Gap = Gap Float


{-| Nodes positions stored as `Graph.NodeId -> Geom.Position` dictionary. -}
type alias NodesPositions = IntDict Geom.Position -- Dict Graph.NodeId Geom.Position


{-| Render the graph as SVG. Takes the function that renders the node (and could render its edges) and the function that returns size for each node. -}
graph
    :  Way (Graph.NodeContext n e)
    -> (Geom.Position -> NodesPositions -> Graph.NodeContext n e -> Svg msg) --
    -> (n -> { width : Float, height : Float })
    -> Graph n e
    -> Svg msg
graph =
    graphWithDefs D.noDefs


{-| Render the graph as SVG. Same as above but you can also provide `Defs` to put in the `<def>`...`</def>` section of the SVG. Useful for re-usable items like arrows supposed to represent edges of a graph. -}
graphWithDefs
    :  D.Defs msg
    -> Way (Graph.NodeContext n e)
    -> (Geom.Position -> NodesPositions -> Graph.NodeContext n e -> Svg msg) --
    -> (n -> { width : Float, height : Float })
    -> Graph n e
    -> Svg msg
graphWithDefs defs way renderNode sizeOfNode g =
    let
        forest = Graph.dfsForest (G.noParentsNodes g) g
        forestGeom = Geom.make way (.node >> .label >> sizeOfNode) forest -- Geom.addForest (.node >> .label >> sizeOfNode) forest
        positions =
            forestGeom
                |> Geom.fold (\pos ctx list -> ( ctx.node.id, pos ) :: list) []
                |> List.concat
                |> ID.fromList
    in
    Render.geometry
        defs
        (\pos -> renderNode pos positions)
        forestGeom

    {-
    Render.forest
        renderNode
        (.node >> .label >> sizeOfNode)
        <| Graph.dfsForest (G.noParentsNodes g) g
    -}


graph_
    :  D.Defs msg
    -> (Geom.Position -> Graph.NodeContext n e -> Svg msg)
    -> (n -> { width : Float, height : Float })
    -> Graph n e
    -> Svg msg
graph_ defs renderNode sizeOfNode g =
    let
        nodes = g |> Graph.nodes |> List.map .label
        gap = (Gap 40) -- Gap opts.gap
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

            |> ((::) (Svg.defs [] <| D.unDefs defs))
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