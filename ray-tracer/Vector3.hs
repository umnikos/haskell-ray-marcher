module Vector3 (
  Vector3,
  add,
  sub,
  squared_mag,
  mag,
  scalarmult,
  dot,
  cross,
  normalize,
  neg,
  roots,
  xor
) where

type Vector3 = (Float, Float, Float)

add :: Vector3 -> Vector3 -> Vector3
add (x,y,z) (a,b,c) = (a+x, b+y, c+z)

sub :: Vector3 -> Vector3 -> Vector3
sub (a,b,c) (x,y,z) = (a-x, b-y, c-z)

squared_mag :: Vector3 -> Float
squared_mag (x,y,z) = (x*x + y*y + z*z)

mag :: Vector3 -> Float
mag v = sqrt (squared_mag v)

scalarmult :: Vector3 -> Float -> Vector3
scalarmult (x,y,z) c = (x*c, y*c, z*c)

dot :: Vector3 -> Vector3 -> Float
dot (x,y,z) (a,b,c) = x*a + b*y + c*z

cross :: Vector3 -> Vector3 -> Vector3
cross (a,b,c) (x,y,z) = (b*z + c*y, -(a*z + c*x), a*y + b*x)

normalize :: Vector3 -> Vector3
normalize v
  | (mag v) /= 0 = scalarmult v (1 / mag v)
  | otherwise    = (0,0,0)

neg :: Vector3 -> Vector3
neg (x,y,z) = (-x,-y,-z)

roots :: Float -> Float -> Float -> [Float]
roots a b c = let discriminant = b*b - 4*a*c
        in if (discriminant < 0.0) then []
           else [ 0.5 * (-b + sqrt discriminant), 0.5 * (-b - sqrt discriminant) ]

xor :: Bool -> Bool -> Bool
xor True b  = not b
xor False b = b
