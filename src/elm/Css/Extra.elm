module Css.Extra exposing (cx, cy, r, stroke)

import Css exposing (ColorValue, Length)


stroke : ColorValue compatible -> Css.Style
stroke color =
    Css.property "stroke" color.value


r : Length compatible units -> Css.Style
r length =
    Css.property "r" length.value


cx : Length compatible units -> Css.Style
cx length =
    Css.property "cx" length.value


cy : Length compatible units -> Css.Style
cy length =
    Css.property "cy" length.value
