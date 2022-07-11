module Graph.Render.Svg.Defs exposing
    ( Defs, defs, noDefs, unDefs
    )


{-|
@docs Defs, defs, noDefs, unDefs
-}
import Svg exposing (Svg)


-- TODO: produce `<g>` instead of `<svg>` and so defs won't be needed.
{-| SVG items to store in `<def>...</def>`. -}
type Defs msg = Defs (List (Svg msg))


{-| SVG `<def>...</def>`s for reuse when rendering graph. For example, arrows can be stored here. -}
defs : List (Svg msg) -> Defs msg
defs = Defs


{-| No defs. -}
noDefs : Defs msg
noDefs = defs []


{-| Unpack defs. -}
unDefs : Defs msg -> (List (Svg msg))
unDefs (Defs list) = list
