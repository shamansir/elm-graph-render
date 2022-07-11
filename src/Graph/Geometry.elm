module Graph.Geometry exposing
    ( Geometry
    , Position
    , fold
    , none, areaSize, forest
    , ItemSize(..), AreaSize(..)
    )


{-| # Geometry

@docs Geometry, Position, fold, forest

# Sizes

@docs none, areaSize, ItemSize, AreaSize
-}


import Graph.Tree as Tree


{-| The size of the item in the `Tree`, to differ it from `AreaSize`. -}
type ItemSize = ItemSize { width : Float, height : Float }
{-| The size of the complete area taken by `Geometry`, to differ it from `ItemSize`. -}
type AreaSize = AreaSize { width : Float, height : Float }
{-| `Position` of the item on the area -}
type alias Position = { x : Float, y : Float }


{-| Empty area. -}
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


{-| Get size of the geometry area. -}
areaSize : Geometry a -> { width : Float, height : Float }
areaSize ( (AreaSize area), _ ) = area


{-| Geometry is used to store previously-calculated positions of items in `Tree.Forest` as well as the items themselves.

To build up `Geometry a` from `Tree.Forest a`, use one of:

* `Graph.Geometry.Vertical.make` - positions are calculated as the vertical flow from top to bottom;
* `Graph.Geometry.Radial.make` - positions are calculated as spreading radially from center to the radius;
* `Graph.Geometry.make` - combines both ways;
-}
type alias Geometry a = ( AreaSize, Tree.Forest (Position, a) )


{-| Fold `Geometry` into list using items' positions. -}
fold : (Position -> a -> acc -> acc) -> acc -> Geometry a -> List acc
fold foldF acc =
    List.map (Tree.levelOrder (\(pos, a) -> always <| foldF pos a) acc)
        << Tuple.second


{-| Get `Forest` with positions from `Geometry` -}
forest : Geometry a -> Tree.Forest (Position, a)
forest = Tuple.second