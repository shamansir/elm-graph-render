module Graph.Tree.Geometry exposing
    ( Geometry
    , Position
    , add
    , fold
    , areaSize
    )


import Graph.Tree as Tree


type ItemSize = ItemSize { width : Float, height : Float }

type AreaSize = AreaSize { width : Float, height : Float }

type alias Position = { x : Float, y : Float }


none : AreaSize
none = AreaSize { width = 0, height = 0 }


growHorz : AreaSize -> AreaSize -> AreaSize
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
    }


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


add : (a -> { width : Float, height : Float }) -> Tree.Forest a -> Geometry a
add itemSize =
    let
        addDimensions : Tree.Forest a -> Tree.Forest (ItemSize, a)
        addDimensions = List.map <| Tree.map <| \a -> ( ItemSize <| itemSize a, a )

        addY : Float -> ( Position, a ) -> ( Position, a )
        addY y ( pos, a ) = ( { x = pos.x, y = pos.y + y }, a )

        addX : Float -> ( Position, a ) -> ( Position, a )
        addX x ( pos, a ) = ( { x = pos.x + x, y = pos.y }, a )

        distributeTree :  Tree.Tree ( ItemSize, a ) -> ( AreaSize, Tree.Tree ( Position, a ) )
        distributeTree tree =
            case Tree.root tree of
                Just ( ( ItemSize rootSize, a ), children ) ->

                    let

                        ( AreaSize childrenArea, shiftedChildren ) =
                            children
                                |> List.map distributeTree
                                |> List.foldl
                                    (\( AreaSize childArea, t ) ( AreaSize prevArea, items ) ->
                                        ( AreaSize
                                            { width = prevArea.width + childArea.width
                                            , height = max prevArea.height childArea.height
                                            }
                                        , Tree.map (addX prevArea.width) t :: items
                                        )
                                    ) (none, [])
                                |> Tuple.mapSecond (List.reverse >> List.map (Tree.map <| addY rootSize.height))

                        rootPos =
                            { x =
                                if List.length children > 0 then
                                    childrenArea.width / 2  - rootSize.width / 2
                                else 0
                            , y = 0
                            }

                    in
                        ( AreaSize
                            { width = max childrenArea.width rootSize.width
                            , height = rootSize.height + childrenArea.height
                            }
                        , Tree.inner ( rootPos, a )
                            <| shiftedChildren
                        )

                Nothing ->
                    ( none, Tree.empty )

        distributeTreesOverWidth : List ( AreaSize, Tree.Tree ( Position, a ) ) -> Geometry a
        distributeTreesOverWidth =
            List.foldl
                (\( AreaSize curArea, tree ) ( AreaSize prevArea, list ) ->
                    ( AreaSize { width = prevArea.width + curArea.width, height = max curArea.height prevArea.height }
                    , (tree |> Tree.map (addX prevArea.width)) :: list
                    )
                )
                ( none, [] )
    in
        distributeTreesOverWidth
            << List.map distributeTree
            << addDimensions


fold : (Position -> a -> acc -> acc) -> acc -> Geometry a -> List acc
fold foldF acc =
    List.map (Tree.levelOrder (\(pos, a) -> always <| foldF pos a) acc)
        << Tuple.second
