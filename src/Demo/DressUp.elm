module Demo.DressUp exposing (..)


import Browser
import IntDict as ID exposing (IntDict)
import Html as H exposing (Html)
import Svg as S
import Svg.Attributes as SA

import Graph as G exposing (Graph)
import Graph.Tree.Geometry as Geom
import Graph.Render.Forest as Render
import Graph.Render.Graph as Render


type alias Size = { width: Float, height : Float }
type alias Sizes = IntDict Size


type alias Model = Graph String ()


init : Model
init = dressUp


dressUp : Model -- node labels are strings, edge labels are empty
dressUp =
  let
    nodes =
      [ G.Node 0 "Socks"
      , G.Node 1 "Undershirt"
      , G.Node 2 "Pants"
      , G.Node 3 "Shoes"
      , G.Node 4 "Watch"
      , G.Node 5 "Shirt"
      , G.Node 6 "Belt"
      , G.Node 7 "Tie"
      , G.Node 8 "Jacket"
      ]

    e from to =
      G.Edge from to ()

    edges =
      [ e 0 3 -- socks before shoes
      , e 1 2 -- undershorts before pants
      , e 1 3 -- undershorts before shoes
      , e 2 3 -- pants before shoes
      , e 2 6 -- pants before belt
      , e 5 6 -- shirt before belt
      , e 5 7 -- shirt before tie
      , e 6 8 -- belt before jacket
      , e 7 8 -- tie before jacket
      ]
  in
    G.fromNodesAndEdges nodes edges


size = { width = 60, height = 60 }



renderEdges : Geom.Position -> Render.NodesPositions -> G.Adjacency () -> Html msg
renderEdges from nodesPositions =
    S.g [] << List.map Tuple.second << ID.toList << ID.map
        (\otherNodeId _ ->
            case ID.get otherNodeId nodesPositions of
                Just otherNodePos ->
                    let
                        otherNodeSize = size
                    in S.line
                        [ SA.x1 <| String.fromFloat (from.x + size.width / 2)
                        , SA.y1 <| String.fromFloat (from.y + size.height / 2)
                        , SA.x2 <| String.fromFloat (otherNodePos.x + otherNodeSize.width / 2)
                        , SA.y2 <| String.fromFloat (otherNodePos.y + otherNodeSize.height / 2)
                        , SA.stroke "rgba(0,0,0,0.5)"
                        , SA.strokeWidth "2"
                        ]
                        []
                Nothing ->
                    S.g [] []
        )


renderNode : Geom.Position -> Render.NodesPositions -> G.NodeContext String () -> Html msg
renderNode pos nodesPositions { node, outgoing } =
    S.g
        []
        <| S.g
            [ SA.transform <| "translate(" ++ String.fromFloat pos.x ++ "," ++ String.fromFloat pos.y ++ ")"
            ]
            [
                S.rect
                    [ SA.width <| String.fromFloat size.width
                    , SA.height <| String.fromFloat size.height
                    , SA.stroke "black"
                    , SA.strokeWidth "1"
                    , SA.fill "transparent"
                    ]
                    []
            ,
                S.text_
                    [ SA.transform <| "translate(" ++ String.fromFloat (size.width / 2) ++ "," ++ String.fromFloat (size.height / 2) ++ ")"
                    , SA.dominantBaseline "hanging"
                    , SA.alignmentBaseline "hanging"
                    , SA.textAnchor "middle"
                    , SA.fontSize "15"
                    , SA.fill "gray"
                    ]
                    [ S.text node.label
                    ]
            ]
        :: renderEdges pos nodesPositions outgoing
        :: []


view : Model -> Html msg
view model =
    Render.graph
        Render.defaultOptions
        renderNode
        (always size)
        model



main =
    Browser.sandbox
        { init = init
        , update = always identity
        , view = view
        }