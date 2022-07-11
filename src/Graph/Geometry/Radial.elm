module Graph.Geometry.Radial exposing (Options, Force, defaultOptions, make)


{-|
@docs make

# Options

@docs Options, defaultOptions, Force
-}

import Graph.Tree as Tree
import Graph.Geometry exposing (..)
import Html.Attributes exposing (width)


type alias Radius = Float


type alias Degree = Float


type alias Levels = Int


type alias Sector = { minDegree : Degree, maxDegree : Degree }


type alias Ring = { minRadius : Radius, maxRadius : Radius }


{-| Some vecrtor direction. -}
type alias Force = { x : Float, y : Float }


{-| -}
type alias Options a =
    { distanceBetweenRings : Float
    , focusPoint : Maybe { x : Float, y : Float }
    , zoom : Float
    , forces : Maybe (a -> List Force)
    }


{-| -}
defaultOptions : Options a
defaultOptions =
    { distanceBetweenRings = 70
    , focusPoint = Nothing
    , zoom = 1.0
    , forces = Nothing
    }


fullCircle : Sector
fullCircle = { minDegree = 0, maxDegree = 360 }


applyForces :  List Force -> { x : Float, y : Float } ->{ x : Float, y : Float }
applyForces fs pos =
    List.foldl (\np p -> { x = np.x + p.x, y = np.y + p.y }) pos fs


zoomPos : Float -> { x : Float, y : Float } -> { x : Float, y : Float }
zoomPos z { x, y } = { x = x * z, y = y * z }


zoomArea : Float -> { width : Float, height : Float } -> { width : Float, height : Float }
zoomArea z { width, height } = { width = width * z, height = height * z }


{-| Calculate radial geometry of the given `Forest a`, using the function that provides size for each item. -}
make : Options a -> (a -> { width : Float, height : Float }) -> Tree.Forest a -> Geometry a
make opts itemSize forest =
    let
        addDimensions : Tree.Forest a -> Tree.Forest (ItemSize, a)
        addDimensions = List.map <| Tree.map <| \a -> ( ItemSize <| itemSize a, a )
        countLevels : Tree.Tree a -> Levels
        countLevels tree =
            case Tree.root tree of
                Just ( _, children ) ->
                    if List.length children > 0 then 1 + countForestLevels children else 0
                Nothing ->
                    0
        countForestLevels : Tree.Forest a -> Levels
        countForestLevels f =
            Maybe.withDefault 0 <| List.maximum <| List.map countLevels f
        levelsCount : Levels
        levelsCount = 1 + countForestLevels forest

        fullRing : Ring
        fullRing = { minRadius = 0, maxRadius = toFloat levelsCount }
        area =

            { width = fullRing.maxRadius * opts.distanceBetweenRings * 2
            , height = fullRing.maxRadius * opts.distanceBetweenRings * 2
            } |> zoomArea opts.zoom
        center =
            case opts.focusPoint of
                Just focus ->
                    { x = area.width / 2 - focus.x
                    , y = area.height / 2 - focus.y
                    }
                Nothing ->
                    { x = area.width / 2
                    , y = area.height / 2
                    }
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
                                                    { x = center.x + (ring.minRadius * opts.distanceBetweenRings * cos (degrees rootAngle)) - (rootSize.width / 2)
                                                    , y = center.y + (ring.minRadius * opts.distanceBetweenRings * sin (degrees rootAngle)) - (rootSize.height / 2)
                                                    } |>
                                                        ( case opts.forces of
                                                            Just fs -> applyForces <| fs a
                                                            Nothing -> identity
                                                        )
                                                      |> zoomPos opts.zoom
                                                , a
                                                )
                                Nothing -> Tree.empty
                        )
            in
                distributedBySector


    in
        ( AreaSize area
        , distributeForest fullRing fullCircle
            <| addDimensions
            <| forest
        )
