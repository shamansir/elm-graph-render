module Graph.Tree.Geometry.Radial exposing (..)


import Graph.Tree as Tree
import Graph.Tree.Geometry exposing (..)


type alias Radius = Float


type alias Degree = Float


type alias Levels = Int


type alias Sector = { minDegree : Degree, maxDegree : Degree }


type alias Ring = { minRadius : Radius, maxRadius : Radius }



fullCircle : Sector
fullCircle = { minDegree = 0, maxDegree = 360 }


addForest : (a -> { width : Float, height : Float }) -> Tree.Forest a -> Geometry a
addForest itemSize =
    let
        addDimensions : Tree.Forest a -> Tree.Forest (ItemSize, a)
        addDimensions = List.map <| Tree.map <| \a -> ( ItemSize <| itemSize a, a )

        addY : Float -> ( Position, a ) -> ( Position, a )
        addY y ( pos, a ) = ( { x = pos.x, y = pos.y + y }, a )

        addX : Float -> ( Position, a ) -> ( Position, a )
        addX x ( pos, a ) = ( { x = pos.x + x, y = pos.y }, a )

        distributeForest : Sector -> Tree.Forest ( ItemSize, a ) -> ( Ring, Tree.Forest ( Position, a ) )
        distributeForest sector forest =
            ( { minRadius = 0, maxRadius = 100 }, [] )


        {- distributeTree : Position -> Sector -> Tree.Tree ( ItemSize, a ) -> ( Sector, Tree.Tree ( Position, a ) )
        distributeTree rootPos sector tree =
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

        distributeTreesOverRadius : List ( AreaSize, Tree.Tree ( Position, a ) ) -> Geometry a
        distributeTreesOverRadius =
            List.foldl
                (\( AreaSize curArea, tree ) ( AreaSize prevArea, list ) ->
                    ( AreaSize { width = prevArea.width + curArea.width, height = max curArea.height prevArea.height }
                    , (tree |> Tree.map (addX prevArea.width)) :: list
                    )
                )
                ( none, [] ) -}


        findAreaSize : ( Ring, Tree.Forest ( Position, a ) ) -> Geometry a
        findAreaSize =
            Tuple.mapFirst (\ring -> AreaSize { width = ring.maxRadius, height = ring.maxRadius })

    in
        findAreaSize
            << distributeForest fullCircle
            << addDimensions
