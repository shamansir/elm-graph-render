module Graph.Render.Svg.Forest exposing
    ( forest, geometry
    )

{-|

# Rendering

@docs forest, geometry
-}

import Graph.Tree as Tree
import Svg exposing (Svg)
import Svg.Attributes as Svg
import Html exposing (Html)

import Graph.Geometry as G
import Graph.Geometry.Make exposing (Way)
import Graph.Geometry.Make as Geometry

import Graph.Render.Svg.Defs exposing (Defs, unDefs)

{-| Render precalculated Geometry (apply positions of the items) using the rendering function. -}
geometry : Defs msg -> (G.Position -> a -> Svg msg) -> G.Geometry a -> Svg msg
geometry defs_ renderItem geom =
    let
        area = G.areaSize geom

        foldRender : G.Position -> a -> List (Svg msg) -> List (Svg msg)
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


{-| Calculate `Geometry` of the `Forest` using the function that returns size of ech item, and then render it using the rendering function -}
forest : Defs msg -> Way a -> (G.Position -> a -> Svg msg) -> (a -> { width : Float, height : Float }) -> Tree.Forest a -> Svg msg
forest defs_ way renderItem itemSize =
    -- let geometry = G.add itemSize f
    -- in ( geometry, forestGeometry renderItem geometry )
    geometry defs_ renderItem
    << Geometry.make way itemSize