module Color(
  Color,
  red, green, blue, white, black, midgrey, nearlywhite,
  scale_col,
  add_col,
  clamp,
  combine_col
) where

type Color = (Float, Float, Float)

red, green, blue, white, black, midgrey, nearlywhite :: Color
red     = (1.0, 0.0, 0.0)
green   = (0.0, 1.0, 0.0)
blue    = (0.0, 0.0, 1.0)
white   = (1.0, 1.0, 1.0)
black   = (0.0, 0.0, 0.0)
midgrey = (0.5, 0.5, 0.5)
nearlywhite = (0.8,0.8,0.8)

scale_col :: Color -> Float -> Color
scale_col (r,g,b) k = (r*k, g*k, b*k)

add_col :: Color -> Color -> Color
add_col (r1,g1,b1) (r2,g2,b2) = (r1+r2, g1+g2, b1+b2)

clamp :: Color -> Color
clamp (r,g,b) = ( clampfloat r, clampfloat g, clampfloat b)
                where clampfloat f = (max 0.0 (min 1.0 f))

combine_col :: Color -> Color -> Color
combine_col (r1,g1,b1) (r2,g2,b2) = (r1*r2, g1*g2, b1*b2)
