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
addForest itemSize forest =
    let
        addDimensions : Tree.Forest a -> Tree.Forest (ItemSize, a)
        addDimensions = List.map <| Tree.map <| \a -> ( ItemSize <| itemSize a, a )
        countLevels tree =
            case Tree.root tree of
                Just ( _, children ) ->
                    if List.length children > 0 then 1 + countForestLevels children else 0
                Nothing ->
                    0
        countForestLevels f =
            Maybe.withDefault 0 <| List.maximum <| List.map countLevels f
        levelsCount = 1 + countForestLevels forest

        -- ring = { minRadius = 0, maxRadius = 200 }
        distanceBetweenRings = 70
        fullRing = { minRadius = 0, maxRadius = levelsCount }
        area =

            { width = fullRing.maxRadius * distanceBetweenRings * 2
            , height = fullRing.maxRadius * distanceBetweenRings * 2
            }
        center = { x = area.width / 2, y = area.height / 2 }

        distributeForest : Ring -> Sector -> Tree.Forest ( ItemSize, a ) -> Tree.Forest ( Position, a )
        distributeForest ring sector f =
            let
                extractRoot tree =
                    Tree.root tree |> Maybe.map Tuple.first
                onlyRoots = f |> List.map extractRoot

                rootsCount = onlyRoots |> List.filterMap identity |> List.length
                perRoot = (sector.maxDegree - sector.minDegree) / toFloat rootsCount
                distributedBySector : List ( Tree.Tree ( Position, a ) )
                distributedBySector =
                    f |> List.indexedMap
                        (\idx tree ->
                            case Tree.root tree of
                                Just ( ( ItemSize rootSize, a ), children ) ->
                                    let
                                        treeSector =
                                            { minDegree = sector.minDegree + toFloat idx * perRoot
                                            , maxDegree = sector.minDegree + (toFloat idx + 1) * perRoot
                                            }
                                        rootAngle =
                                            treeSector.minDegree + (treeSector.maxDegree - treeSector.minDegree) / 2
                                    in
                                        distributeForest
                                            { minRadius = ring.minRadius + 1
                                            , maxRadius = ring.maxRadius
                                            }
                                            treeSector
                                            children
                                            |> Tree.inner
                                                (
                                                    { x = center.x + (ring.minRadius * distanceBetweenRings * cos (degrees rootAngle)) - (rootSize.width / 2)
                                                    , y = center.y + (ring.minRadius * distanceBetweenRings * sin (degrees rootAngle)) - (rootSize.height / 2)
                                                    }
                                                , a
                                                )
                                Nothing -> Tree.empty
                        )
            in
                distributedBySector


    in
        ( AreaSize
            { width = fullRing.maxRadius * distanceBetweenRings * 2
            , height = fullRing.maxRadius * distanceBetweenRings * 2
            }
        , distributeForest fullRing fullCircle
            <| addDimensions
            <| forest
        )
