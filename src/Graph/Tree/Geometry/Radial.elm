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

        center = { x = 200, y = 200 }

        -- ring = { minRadius = 0, maxRadius = 200 }
        distance = 20

        distributeForest : Sector -> Tree.Forest ( ItemSize, a ) -> ( Ring, Tree.Forest ( Position, a ) )
        distributeForest sector forest =
            let
                extractRoot tree =
                    Tree.root tree |> Maybe.map Tuple.first
                onlyRoots = forest |> List.map extractRoot

                rootsCount = onlyRoots |> List.filterMap identity |> List.length
                perRoot = (sector.maxDegree - sector.minDegree) / toFloat rootsCount
                {-
                countLevels n tree =
                    case Tree.root tree of
                        Just ( _, children ) ->
                            n + 1 + countForestLevels children
                        Nothing ->
                            n
                countForestLevels f =
                    List.foldl (+) 0 <| List.map (countLevels 0) f
                -}
                emptyRing = { minRadius = 0, maxRadius = 0 }
                distributedBySector : List ( Ring, Tree.Tree ( Position, a ) )
                distributedBySector =
                    forest |> List.indexedMap
                        (\idx tree ->
                            case Tree.root tree of
                                Just ( ( ItemSize rootSize, a ), children ) ->
                                    let
                                        treeSector =
                                            { minDegree = toFloat idx * perRoot
                                            , maxDegree = (toFloat idx + 1) * perRoot
                                            }
                                        rootAngle =
                                            (treeSector.maxDegree - treeSector.minDegree) / 2
                                    in
                                        case distributeForest treeSector children of
                                            ( iring, innerForest )
                                                ->
                                                    (
                                                        { minRadius = 0
                                                        , maxRadius = iring.maxRadius + 1
                                                        }
                                                    , Tree.inner
                                                        (
                                                            { x = center.x + (iring.minRadius * cos (degrees rootAngle)) - (rootSize.width / 2)
                                                            , y = center.y + (iring.minRadius * sin (degrees rootAngle)) - (rootSize.height / 2)
                                                            }
                                                        , a
                                                        )
                                                        innerForest
                                                    )
                                Nothing -> ( emptyRing, Tree.empty )
                        )
            in
                ( distributedBySector
                    |> List.map Tuple.first
                    |> List.foldl
                            (\iring prevRing ->
                                { minRadius = min iring.minRadius prevRing.minRadius
                                , maxRadius = max iring.maxRadius prevRing.maxRadius
                                }
                            )
                            { minRadius = 0, maxRadius = 100 }
                , distributedBySector |> List.map Tuple.second
                )


        findAreaSize : ( Ring, Tree.Forest ( Position, a ) ) -> Geometry a
        findAreaSize =
            Tuple.mapFirst (\iring -> AreaSize { width = iring.maxRadius * 2, height = iring.maxRadius * 2 })

    in
        findAreaSize
            << distributeForest fullCircle
            << addDimensions
