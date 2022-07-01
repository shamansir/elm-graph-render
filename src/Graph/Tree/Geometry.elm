module Graph.Tree.Geometry exposing
    ( Geometry
    , Position
    , none
    , fold
    , areaSize
    , ItemSize(..), AreaSize(..)
    )


import Graph.Tree as Tree


type ItemSize = ItemSize { width : Float, height : Float }

type AreaSize = AreaSize { width : Float, height : Float }

type alias Position = { x : Float, y : Float }


none : AreaSize
none = AreaSize { width = 0, height = 0 }


{- growHorz : AreaSize -> AreaSize -> AreaSize
growHorz (AreaSize dimA) (AreaSize dimB) =
    AreaSize
    { width = dimA.width + dimB.width
    , height = max dimA.height dimB.height
    }


growVert : AreaSize -> AreaSize -> AreaSize
growVert (AreaSize dimA) (AreaSize dimB) =
    AreaSize
    { width = max dimA.width dimB.width
    , height = dimA.height + dimB.height
    } -}


foldMapForest : (a -> Tree.Forest a -> acc -> ( acc, b )) -> acc -> Tree.Forest a -> ( acc, Tree.Forest b )
foldMapForest f acc =
    List.foldl
            (\childTree (prevAcc, nextForest) ->
                case foldMapTree f prevAcc childTree of
                    ( nextAcc, tree ) ->
                        ( nextAcc, tree :: nextForest )
            )
            (acc, [])
        >> Tuple.mapSecond List.reverse


foldMapTree : (a -> Tree.Forest a -> acc -> ( acc, b )) -> acc -> Tree.Tree a -> ( acc, Tree.Tree b )
foldMapTree f acc tree =
    case Tree.root tree of
        Just ( a, children ) ->
            case f a children acc of
                ( nextAcc, b ) ->
                    let
                        ( childrenAppliedAcc, nextChildren ) =
                            children
                                |> foldMapForest f nextAcc
                    in

                    ( childrenAppliedAcc, Tree.inner b <| nextChildren )
        Nothing ->
            ( acc, Tree.empty )


areaSize : Geometry a -> { width : Float, height : Float }
areaSize ( (AreaSize area), _ ) = area


type alias Geometry a = ( AreaSize, Tree.Forest (Position, a) )


fold : (Position -> a -> acc -> acc) -> acc -> Geometry a -> List acc
fold foldF acc =
    List.map (Tree.levelOrder (\(pos, a) -> always <| foldF pos a) acc)
        << Tuple.second
