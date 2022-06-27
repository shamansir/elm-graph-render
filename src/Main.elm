module Main exposing (..)


import Browser
import Html exposing (..)


import Graph exposing (Graph)
import Array exposing (Array)


type MyTree a
    = Leaf a
    | Branch (Array (MyTree a))


type alias Model = { focus : Maybe (List Int), tree : MyTree (Maybe ()) }


type Msg
    = Select (List Int)
    | AddLeaf (List Int)
    | AddBranch (List Int)



init : Model
init = { focus = Nothing, tree = Leaf Nothing }


update : Msg -> Model -> Model
update _ model = model


view : Model -> Html Msg
view model = div [] []


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


foldMyTree : (a -> b -> b) -> b -> MyTree a -> b
foldMyTree f b tree =
    case tree of
        Leaf a -> f a b
        Branch array ->
            Array.foldl
                (\it ib -> foldMyTree f ib it)
                b array


myTreeToGraph : MyTree a -> Graph a ()
myTreeToGraph _ = Graph.empty