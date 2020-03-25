module Vector
  ( Vec3(..)
  , dot
  , scale
  , mag
  , normalize
  ) where

data Vec3 = Vec3 !Double !Double !Double deriving (Show,Eq)

instance Num Vec3 where
    (Vec3 x y z) + (Vec3 x1 y1 z1) = Vec3 (x + x1) (y + y1) (z + z1)
    negate (Vec3 x y z) = Vec3 (-x) (-y) (-z)
    x - y = x + negate y
    abs (Vec3 x y z) = Vec3 (abs x) (abs y) (abs z)
    (Vec3 x y z) * (Vec3 x1 y1 z1) = Vec3 (x * x1) (y * y1) (z * z1)
    fromInteger x = let y = fromInteger x in Vec3 y y y
    signum (Vec3 x y z) = Vec3 (signum x) (signum y) (signum z)


-- | The dot product of two vectors
(Vec3 x y z) `dot` (Vec3 a s d) = x*a + y*s + z*d
-- | Scale a vector's magnitude by a number.
a `scale` (Vec3 x y z) = Vec3 (a*x) (a*y) (a*z)

-- | (See 'mag').
squared_mag :: Vec3 -> Double
squared_mag v3@(Vec3 x y z) = (x * x + y * y + z * z)

-- | Returns the magnitude (i.e. the length) of a vector.
mag :: Vec3 -> Double
mag v = sqrt (squared_mag v)

-- | Scale a vector so that it will have a magnitude of 1
normalize :: Vec3 -> Vec3
normalize (Vec3 0 0 0) = error "Cannot normalize a vector with magnitude 0"
normalize v = ( 1 / mag v) `scale` v