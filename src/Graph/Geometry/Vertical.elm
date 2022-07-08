module Graph.Geometry.Vertical exposing (Options, defaultOptions, make)


import Graph.Tree as Tree
import Graph.Geometry exposing (..)


type alias Options = ()


defaultOptions : Options
defaultOptions = ()


make : Options -> (a -> { width : Float, height : Float }) -> Tree.Forest a -> Geometry a
make _ itemSize =
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
                                    max 0 (childrenArea.width / 2 - rootSize.width / 2)
                                else 0
                            , y = 0
                            }

                        moveToCenter =
                            List.map <| Tree.map (addX <| (rootSize.width - childrenArea.width) / 2)

                    in
                        ( AreaSize
                            { width = max childrenArea.width rootSize.width
                            , height = rootSize.height + childrenArea.height
                            }
                        , Tree.inner ( rootPos, a )
                            <| if childrenArea.width >= rootSize.width
                                then shiftedChildren
                                else shiftedChildren |> moveToCenter
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