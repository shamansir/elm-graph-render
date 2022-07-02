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

        ring = { minRadius = 0, maxRadius = 200 }

        distributeForest : Sector -> Tree.Forest ( ItemSize, a ) -> ( Ring, Tree.Forest ( Position, a ) )
        distributeForest sector forest =
            let
                extractRoot tree =
                    Tree.root tree |> Maybe.map Tuple.first
                onlyRoots = forest |> List.map extractRoot

                rootsCount = onlyRoots |> List.filterMap identity |> List.length
                perRoot = (sector.maxDegree - sector.minDegree) / toFloat rootsCount
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
                                        Tree.inner
                                            (
                                                { x = center.x + (ring.maxRadius * cos (degrees rootAngle)) + (rootSize.width / 2)
                                                , y = center.y + (ring.maxRadius * sin (degrees rootAngle)) + (rootSize.height / 2)
                                                }
                                            , a
                                            )
                                            <| Tuple.second
                                            <| distributeForest treeSector children
                                Nothing -> Tree.empty
                        )
            in
                ( ring, distributedBySector )


        findAreaSize : ( Ring, Tree.Forest ( Position, a ) ) -> Geometry a
        findAreaSize =
            Tuple.mapFirst (\iring -> AreaSize { width = iring.maxRadius * 2, height = iring.maxRadius * 2 })

    in
        findAreaSize
            << distributeForest fullCircle
            << addDimensions
