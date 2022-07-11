
module Graph.Geometry.Make exposing
    ( make
    , defaultWay
    , vertical, radial, Way(..))

{-|
@docs make

# Way

@docs Way, defaultWay, vertical, radial
-}


import Graph.Tree as Tree

import Graph.Geometry as G
import Graph.Geometry.Vertical as GV
import Graph.Geometry.Radial as GR


{-| The way graph is distributed.

For `Vertical` way, provide options from `Graph.Geometry.Vertical`, and for `Radial`, the ones
from `Graph.Geometry.Radial`, correspondingly.
-}
type Way a
    = Vertical GV.Options
    | Radial (GR.Options a)


{-| -}
defaultWay : Way a
defaultWay = Vertical GV.defaultOptions


{-| -}
vertical : GV.Options -> Way a
vertical = Vertical


{-| -}
radial : GR.Options a -> Way a
radial = Radial


{-| Distribute the forest over the area: calculate positions for all the items in the requested way and the area they will take.
As the next step `Geometry` is provided to the rendering function to actually apply it.  -}
make : Way a -> (a -> { width : Float, height : Float }) -> Tree.Forest a -> G.Geometry a
make way itemSize =
    case way of
        Vertical vopts -> GV.make vopts itemSize
        Radial ropts -> GR.make ropts itemSize