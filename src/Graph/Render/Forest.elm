module Graph.Render.Forest exposing (..)

import Graph.Tree as Tree
import Svg exposing (Svg)
import Svg.Attributes as Svg
import Html exposing (Html)
import Graph.Tree.Geometry as G
import Graph.Tree.Geometry.Vertical as G


type Defs msg = Defs (List (Svg msg))


defs : List (Svg msg) -> Defs msg
defs = Defs


noDefs : Defs msg
noDefs = defs []


unDefs : Defs msg -> (List (Svg msg))
unDefs (Defs list) = list


forestGeometry : Defs msg -> (G.Position -> a -> Html msg) -> G.Geometry a -> Html msg
forestGeometry defs_ renderItem geom =
    let
        area = G.areaSize geom

        foldRender : G.Position -> a -> List (Html msg) -> List (Html msg)
        foldRender pos a list =
            renderItem pos a :: list

    in
        Svg.svg
                [ Svg.width <| String.fromFloat <| area.width + 5
                , Svg.height <| String.fromFloat <| area.height + 5
                ]
            <| ((::) (Svg.defs [] <| unDefs defs_))
            <| List.singleton
            <| Svg.g []
            <| List.concat
            <| G.fold foldRender []
            <| geom


forest : Defs msg -> (G.Position -> a -> Html msg) -> (a -> { width : Float, height : Float }) -> Tree.Forest a -> Html msg
forest defs_ renderItem itemSize =
    -- let geometry = G.add itemSize f
    -- in ( geometry, forestGeometry renderItem geometry )
    forestGeometry defs_ renderItem
    << G.addForest itemSize