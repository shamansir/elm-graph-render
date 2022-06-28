module Main exposing (..)


import Browser
import Html exposing (..)
import Svg
import Svg.Attributes as Svg


import Graph exposing (Graph)
import Array exposing (Array)

import Graph.Tree.Geometry as Geom
import Graph.Render.Graph as Render


type alias Path = List Int


type MyTree a
    = Leaf a
    | Branch (Array (MyTree a))


type alias Model =
    { focus : Maybe (List Int)
    , tree : MyTree Condition
    , graph : Graph Condition ()
    }


type Condition
    = NotDetermined
    | SetToBeLeaf
    | SetToBeBranch


type Msg
    = Select (List Int)
    | ConvertToLeaf (List Int)
    | ConvertToBranch (List Int)


init : Model
init =
    let
        tree = Leaf NotDetermined
    in
        { focus = Nothing
        , tree = tree
        , graph = myTreeToGraph tree
        }


update : Msg -> Model -> Model
update _ model = model



nodeCtx : Geom.Position -> Graph.NodeContext Condition () -> Html Msg
nodeCtx { x, y } { node } =
     Svg.text_
        [ Svg.transform <| translateTo x y
        , Svg.dominantBaseline "hanging"
        , Svg.alignmentBaseline "hanging"
        ]
        [ text <|
            case node.label of
                NotDetermined -> "not determined"
                SetToBeLeaf -> "leaf"
                SetToBeBranch -> "branch"
        ]


view : Model -> Html Msg
view model =
    div
        [ ]
        [ Render.graph
            Render.defaultOptions
            (\pos nodes ctx -> nodeCtx pos ctx)
            (always { width = 40, height = 40})
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
        )


foldMyTree_ : (MyTree a -> b -> b) -> b -> MyTree a -> b
foldMyTree_ =
    foldMyTreeWithPath << always


foldMyTreeWithPath : (Path -> MyTree a -> b -> b) -> b -> MyTree a -> b
foldMyTreeWithPath f =
    let
        innerFold ip ib itree =
            case itree of
                Leaf _ -> f ip itree ib
                Branch array ->
                    Array.foldl
                        (\(idx, iit) iib -> innerFold (ip ++ [ idx ]) iib iit)
                        (innerFold ip ib itree)
                        <| Array.indexedMap Tuple.pair
                        <| array
    in
        innerFold []


myTreeToGraph : MyTree a -> Graph a ()
myTreeToGraph tree =
    let
        foldF path itree ( ns, es ) =
            case itree of
                Leaf a ->
                    ( Graph.Node (pathToId path) a :: ns
                    , es
                    )
                Branch trees ->
                    -- leaves will be visited separately
                    ( ns
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
    in path
        |> List.indexedMap
            (\idx pos -> pos * (100 ^ (pathLen - 1 - idx))) -- FIXME: not very reliable
        |> List.sum


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