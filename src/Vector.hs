module Vector (
  Vec3(..)
  , mag
)
where

data Vec3 = Vec3 !Double !Double !Double deriving (Show,Eq) -- Force evaluation to reduce overhead

instance Num Vec3 where -- Necessary instance for Vector operatins
    (Vec3 x y z) + (Vec3 x1 y1 z1) = Vec3 (x + x1) (y + y1) (z + z1)
    negate (Vec3 x y z) = Vec3 (-x) (-y) (-z)
    x - y = x + negate y
    abs (Vec3 x y z) = Vec3 (abs x) (abs y) (abs z)
    (Vec3 x y z) * (Vec3 x1 y1 z1) = Vec3 (x * x1) (y * y1) (z * z1)
    fromInteger x = let y = fromInteger x in Vec3 y y y
    signum = error "Signum on Vec3 is not implemented"


(Vec3 x y z) `dot` (Vec3 a s d) = x*a + y*s + z*d
(Vec3 x y z) `scale` a = Vec3 (a*x) (a*y) (a*z)

squared_mag :: Vec3 -> Double
squared_mag v3@(Vec3 x y z) = (x * x + y * y + z * z)

mag :: Vec3 -> Double
mag v = sqrt (squared_mag v)
