module Main exposing (..)


import Browser
import Html as Html exposing (Html)
import Svg as S
import Svg.Attributes as SA
import Svg.Events as SE


import Graph exposing (Graph)
import Array exposing (Array)

import Graph.Tree.Geometry as Geom
import Graph.Render.Graph as Render

import IntDict


type alias Path = List Int


type MyTree a
    = Stop
    | Leaf a
    | Branch (Array (MyTree a))


type alias Model =
    { focus : Maybe Path
    , tree : MyTree ()
    , graph : Graph ( Path, Condition ) ()
    }


type Condition
    = IsLeaf
    | IsBranch


type Msg
    = Select Path
    | ConvertToLeaf Path
    | ConvertToBranch Path
    | AddLeaf Path


init : Model
init =
    let
        tree = Leaf ()
    in
        { focus = Nothing
        , tree = tree
        , graph = myTreeToGraph tree
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Select path ->
            { model
            | focus = Just path
            }
        ConvertToLeaf path ->
            let
                nextTree =
                    modifyMyTree
                        (\otherPath t ->
                            if otherPath == path then
                                Just <| Leaf ()
                            else Just t
                        )
                        model.tree
            in
                { model
                | tree = nextTree
                , graph = myTreeToGraph nextTree
                }
        ConvertToBranch path ->
            let
                nextTree =
                    modifyMyTree
                        (\otherPath t ->
                            if otherPath == path then
                                Just <| Branch Array.empty
                            else Just t
                        )
                        model.tree
            in
                { model
                | tree = nextTree
                , graph = myTreeToGraph nextTree
                }
        AddLeaf path ->
            let
                nextTree =
                    modifyMyTree
                        (\otherPath t ->
                            if otherPath == path then
                                Just <| case t of
                                    Branch leaves -> Branch <| Array.push (Leaf ()) leaves
                                    _ -> t
                            else Just t
                        )
                        model.tree
            in
                { model
                | tree = nextTree
                , graph = myTreeToGraph nextTree
                }


renderEdges : Geom.Position -> Render.NodesPositions -> Graph.Adjacency () -> Html Msg
renderEdges from nodesPositions =
    S.g [] << List.map Tuple.second << IntDict.toList << IntDict.map
        (\otherNodeId _ ->
            case IntDict.get otherNodeId nodesPositions of
                Just otherNodePos ->
                    S.line
                        [ SA.x1 <| String.fromFloat from.x
                        , SA.y1 <| String.fromFloat from.y
                        , SA.x2 <| String.fromFloat otherNodePos.x
                        , SA.y2 <| String.fromFloat otherNodePos.y
                        , SA.stroke "rgba(0,0,0,0.5)"
                        , SA.strokeWidth "2"
                        ]
                        []
                Nothing ->
                    S.g [] []
        )


nodeCtx : Render.NodesPositions -> Geom.Position -> Graph.NodeContext ( Path, Condition ) () -> Html Msg
nodeCtx nodesPositions pos { node, outgoing } =
    S.g
        []
        <| S.g
            [ SA.transform <| translateTo pos.x pos.y
            ]
            (
                ( S.rect
                    [ SA.width "70", SA.height "70"
                    , SA.stroke "black"
                    , SA.strokeWidth "1"
                    , SA.fill "transparent"
                    ]
                    []
                )
            :: case node.label of
                    ( path, IsLeaf ) ->
                        [ showId { x = 0, y = 0 } node.id
                        , quickText { x = 25, y = 10 } "🍁"
                        , quickClickableText (ConvertToBranch path) { x = 20, y = 30 } "to 🌿"
                        ]
                    ( path, IsBranch ) ->
                        [ showId { x = 0, y = 0 } node.id
                        , quickText { x = 25, y = 10 } "🌿"
                        , quickClickableText (AddLeaf path) { x = 20, y = 30 } "add 🍁"
                        , quickClickableText (ConvertToLeaf path) { x = 20, y = 45 } "to 🍁"
                        ]
            )
        ::
        case node.label of
            ( _, IsLeaf ) ->
                []
            ( _, IsBranch ) ->
                [ renderEdges pos nodesPositions outgoing ]


view : Model -> Html Msg
view model =
    Html.div
        [ ]
        [ Render.graph
            Render.defaultOptions
            (\pos nodes ctx -> nodeCtx nodes pos ctx)
            (always { width = 70, height = 70 })
            model.graph
        ]


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


translateTo : Float -> Float -> String
translateTo x y = "translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")"


foldMyTree : (a -> b -> b) -> b -> MyTree a -> b
foldMyTree f =
    foldMyTree_
        (\tree prevB ->
            case tree of
                Leaf a -> f a prevB
                Branch _ -> prevB
                Stop -> prevB
        )


foldMyTree_ : (MyTree a -> b -> b) -> b -> MyTree a -> b
foldMyTree_ =
    foldMyTreeWithPath << always


foldMyTreeWithPath : (Path -> MyTree a -> b -> b) -> b -> MyTree a -> b
foldMyTreeWithPath f =
    let
        innerFold ip ib itree =
            case itree of
                Stop -> f ip itree ib
                Leaf _ -> f ip itree ib
                Branch array ->
                    Array.foldl
                        (\(idx, iit) iib -> innerFold (ip ++ [ idx ]) iib iit)
                        (f ip itree ib) {- (innerFold ip ib itree) -}
                        <| Array.indexedMap Tuple.pair
                        <| array
    in
        innerFold []


myTreeToGraph : MyTree () -> Graph ( Path, Condition ) ()
myTreeToGraph tree =
    let
        foldF path itree ( ns, es ) =
            case itree of
                Stop ->
                    ( ns, es )
                Leaf _ ->
                    ( Graph.Node (pathToId path) ( path, IsLeaf ) :: ns
                    , es
                    )
                Branch trees ->
                    -- leaves will be visited separately
                    ( Graph.Node (pathToId path) ( path, IsBranch ) :: ns
                    ,
                        (trees
                        |> Array.indexedMap Tuple.pair
                        |> Array.map Tuple.first
                        |> Array.map
                            (\idx ->
                                Graph.Edge
                                    (pathToId path)
                                    (pathToId <| path ++ [ idx ])
                                    ()
                            )
                        |> Array.toList
                        ) ++ es
                    )
        (nodes, edges) = foldMyTreeWithPath foldF ( [], [] ) tree
    in Graph.fromNodesAndEdges (Debug.log "nodes" nodes) edges



pathToId : List Int -> Int
pathToId path =
    let
        pathLen = List.length path
    in
        if pathLen > 0 then
            path
                |> List.indexedMap
                    (\idx pos -> (pos + 1) * (100 ^ (pathLen - 1 - idx))) -- FIXME: not very reliable
                |> List.sum
        else -1


modifyMyTree : (Path -> MyTree a -> Maybe (MyTree a)) -> MyTree a -> MyTree a
modifyMyTree f =
    let
        modifyF ip itree =
            case itree of
                Branch array ->
                    case f ip
                        <| Branch
                        <| Array.map
                            (\(idx, iit) -> modifyF (ip ++ [ idx ]) iit)
                        <| Array.indexedMap Tuple.pair
                        <| array of
                            Just nextBranch -> nextBranch
                            Nothing -> Stop
                _ ->
                    case f ip itree of
                        Just replacementTree -> replacementTree
                        Nothing -> Stop
    in
        modifyF []


showId : Geom.Position -> Int -> Html msg
showId { x, y } id =
    S.text_
        [ SA.transform <| translateTo x y
        , SA.dominantBaseline "hanging"
        , SA.alignmentBaseline "hanging"
        , SA.fontSize "8"
        , SA.fill "gray"
        ]
        [ S.text <| String.fromInt id
        ]


quickText : Geom.Position -> String -> Html msg
quickText { x, y } string =
    S.text_
        [ SA.transform <| translateTo x y
        , SA.dominantBaseline "hanging"
        , SA.alignmentBaseline "hanging"
        , SA.fontSize "12"
        ]
        [ S.text string
        ]


quickClickableText : msg -> Geom.Position -> String -> Html msg
quickClickableText msg { x, y } string =
    S.text_
        [ SA.transform <| translateTo x y
        , SA.dominantBaseline "hanging"
        , SA.alignmentBaseline "hanging"
        , SA.fontSize "12"
        , SE.onClick msg
        , SA.style "cursor: pointer"
        ]
        [ Html.text string
        ]



{- toParentId : List Int -> Int
toParentId path =
    let
        pathLen = List.length path
    in
        List.take
            (pathLen - 1)
            path
            |> pathToId
-}