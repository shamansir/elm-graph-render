module Demo.Main exposing (..)


import Browser

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Svg as S
import Svg.Attributes as SA
import Svg.Events as SE

import Array exposing (Array)

import Graph exposing (Graph)
import Graph.Tree.Geometry as Geom
import Graph.Render.Graph as Render
import Graph.Render.Forest as FRender
import Graph.Tree.Geometry.Vertical as VR
import Graph.Tree.Geometry.Radial as GR

import IntDict exposing (IntDict)


type alias Path = List Int


type alias Size = { width: Float, height : Float }


type alias Sizes = IntDict Size


type Way = Vertical | Radial


type MyTree a
    = Stop
    | Leaf a
    | Branch (Array (MyTree a))


type alias Model =
    { way : Way
    , focus : Maybe Path
    , tree : MyTree ()
    , graph : Graph ( Path, Condition ) ()
    , sizes : Sizes
    -- , zoom : Float
    -- , focusPoint : Maybe { x : Float, y : Float }
    }


type Condition
    = IsLeaf
    | IsBranch


type Msg
    = Select Path
    | ConvertToLeaf Path
    | ConvertToBranch Path
    | AddLeaf Path
    | Remove Path
    | IncreaseWidth Path
    | IncreaseHeight Path
    | DecreaseWidth Path
    | DecreaseHeight Path
    | ChangeWay Way


init : Model
init =
    let
        tree = Leaf ()
    in
        { way = Vertical
        , focus = Nothing
        , tree = tree
        , graph = myTreeToGraph tree
        , sizes = IntDict.empty |> IntDict.insert -1 defaultSize
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
                -- , sizes = model.sizes |> IntDict.insert (pathToId path) defaultSize
                -- FIXME: that sets the size for parent
                }
        Remove path ->
            let
                nextTree =
                    modifyMyTree
                        (\otherPath t ->
                            if otherPath == path then
                                Nothing -- Just Stop
                            else Just t
                        )
                        model.tree
            in
                { model
                | tree = nextTree
                , graph = myTreeToGraph nextTree
                , sizes = model.sizes |> IntDict.remove (pathToId path)
                }
        IncreaseWidth path ->
            { model
            | sizes =
                IntDict.update
                    (pathToId path)
                    (\v -> case v of
                        Just { width, height } -> Just { width = min 150 <| width + 10, height = height }
                        Nothing -> Just { defaultSize | width = defaultSize.width + 10 }
                    )
                    model.sizes
            }
        IncreaseHeight path ->
            { model
            | sizes =
                IntDict.update
                    (pathToId path)
                    (\v -> case v of
                        Just { width, height } -> Just { width = width, height = min 150 <| height + 10 }
                        Nothing -> Just { defaultSize | height = defaultSize.height + 10 }
                    )
                    model.sizes
            }
        DecreaseWidth path ->
            { model
            | sizes =
                IntDict.update
                    (pathToId path)
                    (\v -> case v of
                        Just { width, height } -> Just { width = max 10 <| width - 10, height = height }
                        Nothing -> Just { defaultSize | width = defaultSize.width - 10 }
                    )
                    model.sizes
            }
        DecreaseHeight path ->
            { model
            | sizes =
                IntDict.update
                    (pathToId path)
                    (\v -> case v of
                        Just { width, height } -> Just { width = width, height = max 10 <| height - 10 }
                        Nothing -> Just { defaultSize | height = defaultSize.height - 10 }
                    )
                    model.sizes
            }
        ChangeWay way ->
            { model
            | way = way
            }


renderEdges : Size -> Geom.Position -> Sizes -> Render.NodesPositions -> Graph.Adjacency () -> Html Msg
renderEdges size from sizes nodesPositions =
    S.g [] << List.map Tuple.second << IntDict.toList << IntDict.map
        (\otherNodeId _ ->
            -- case Maybe.map2 Tuple.pair (IntDict.get otherNodeId nodesPositions) (IntDict.get otherNodeId sizes) of
            case IntDict.get otherNodeId nodesPositions of
                Just otherNodePos ->
                    let
                        otherNodeSize = IntDict.get otherNodeId sizes
                                            |> Maybe.withDefault defaultSize
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


nodeCtx : Sizes -> Render.NodesPositions -> Geom.Position -> Graph.NodeContext ( Path, Condition ) () -> Html Msg
nodeCtx sizes nodesPositions pos { node, outgoing } =
    let
        size =
            sizes
                |> IntDict.get (pathToId <| Tuple.first <| node.label)
                |> Maybe.withDefault defaultSize
    in S.g
        []
        <| S.g
            [ SA.transform <| translateTo pos.x pos.y
            ]
            (
                ( S.rect
                    [ SA.width <| String.fromFloat size.width
                    , SA.height <| String.fromFloat size.height
                    , SA.stroke "black"
                    , SA.strokeWidth "1"
                    , SA.fill "transparent"
                    ]
                    []
                )
            :: case node.label of
                    ( path, IsLeaf ) ->
                        [ showId { x = 5, y = 5 } node.id
                        , quickText { x = 25, y = 10 } "🍁"
                        , quickClickableText (ConvertToBranch path) { x = 20, y = 30 } "to 🌿"
                        , quickClickableText (IncreaseWidth path) { x = 10, y = 55 } "+w"
                        , quickClickableText (DecreaseWidth path) { x = 25, y = 55 } "-w"
                        , quickClickableText (IncreaseHeight path) { x = 40, y = 55 } "+h"
                        , quickClickableText (DecreaseHeight path) { x = 55, y = 55 } "-h"
                        ]
                    ( path, IsBranch ) ->
                        [ showId { x = 5, y = 5 } node.id
                        , quickText { x = 25, y = 10 } "🌿"
                        , quickClickableText (AddLeaf path) { x = 20, y = 30 } "add 🍁"
                        , quickClickableText (ConvertToLeaf path) { x = 20, y = 45 } "to 🍁"
                        , quickClickableText (IncreaseWidth path) { x = 10, y = 55 } "+w"
                        , quickClickableText (DecreaseWidth path) { x = 25, y = 55 } "-w"
                        , quickClickableText (IncreaseHeight path) { x = 40, y = 55 } "+h"
                        , quickClickableText (DecreaseHeight path) { x = 55, y = 55 } "-h"
                        ]
            )
        ::
        case node.label of
            ( _, IsLeaf ) ->
                []
            ( _, IsBranch ) ->
                [ renderEdges size pos sizes nodesPositions outgoing ]


defaultSize = { width = 70, height = 70 }


view : Model -> Html Msg
view model =
    H.div
        [ ]
        [ Render.graph
            ( FRender.noDefs
            , case model.way of
                Vertical ->
                    FRender.Vertical VR.defaultOptions
                Radial ->
                    FRender.Radial GR.defaultOptions
            )
            (\pos nodes ctx ->
                nodeCtx model.sizes nodes pos ctx
            )
            (\(path, _) ->
                case IntDict.get (pathToId path) model.sizes of
                    Just nodeSize -> nodeSize
                    Nothing -> defaultSize
            )
            model.graph
        , H.div
            [ HA.style "position" "absolute"
            , HA.style "right" "0"
            , HA.style "top" "0"
            ]
            [ H.button [ HE.onClick <| ChangeWay Vertical ] [ H.text "Vertical" ]
            , H.button [ HE.onClick <| ChangeWay Radial ] [ H.text "Radial" ]
            ]
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
    in Graph.fromNodesAndEdges nodes edges



-- TODO: clear sizes using this function when converting branch to a leaf or removing node
childrenAt : Path -> MyTree a -> List ( Path, Condition )
childrenAt path t =
    let
        visitNode : Path -> MyTree a -> List ( Path, Condition )
        visitNode rpath node =
            if List.isEmpty rpath then
                case node of
                    Leaf _ -> [ ( path, IsLeaf ) ]
                    Branch leaves -> ( path, IsBranch ) :: (List.concat <| Array.toList <| Array.map (visitNode rpath) leaves)
                    Stop -> []
            else
                case node of
                    Leaf _ -> [] -- no children at path
                    Branch leaves ->
                        let
                            reversedPath =  List.reverse path
                            maybeTopIdx = List.head reversedPath
                        in
                            case maybeTopIdx of
                                Just topIdx ->
                                    case Maybe.map2 Tuple.pair (Array.get topIdx leaves) (List.tail reversedPath) of
                                        Just ( lnode, reversedTail ) ->
                                            visitNode (List.reverse reversedTail) lnode
                                        Nothing -> []
                                Nothing -> []

                    Stop -> [] -- no children at path
    in visitNode path t


pathToId : Path -> Int
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
        [ S.text string
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