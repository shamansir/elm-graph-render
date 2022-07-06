module Graph.Render.Forest exposing
    ( Options, Way
    , defaultOptions
    , defs, noDefs, unDefs, forest, forestGeometry
    , makeGeometry
    )

import Graph.Tree as Tree
import Svg exposing (Svg)
import Svg.Attributes as Svg
import Html exposing (Html)

import Graph.Tree.Geometry as G
import Graph.Tree.Geometry.Vertical as GV
import Graph.Tree.Geometry.Radial as GR


type Way a
    = Vertical GV.Options
    | Radial (GR.Options a)


type Defs msg = Defs (List (Svg msg))


defs : List (Svg msg) -> Defs msg
defs = Defs


noDefs : Defs msg
noDefs = defs []


unDefs : Defs msg -> (List (Svg msg))
unDefs (Defs list) = list


type alias Options msg a = ( Defs msg, Way a )


defaultOptions : Options msg a
defaultOptions = ( noDefs, Vertical GV.defaultOptions )


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


forest : Options msg a -> (G.Position -> a -> Html msg) -> (a -> { width : Float, height : Float }) -> Tree.Forest a -> Html msg
forest ( defs_, way ) renderItem itemSize =
    -- let geometry = G.add itemSize f
    -- in ( geometry, forestGeometry renderItem geometry )
    forestGeometry defs_ renderItem
    << makeGeometry ( defs_, way ) itemSize


makeGeometry : Options msg a -> (a -> { width : Float, height : Float }) -> Tree.Forest a -> G.Geometry a
makeGeometry ( defs_, way ) itemSize =
    case way of
            Vertical vopts -> GV.addForest vopts itemSize
            Radial ropts -> GR.addForest ropts itemSize