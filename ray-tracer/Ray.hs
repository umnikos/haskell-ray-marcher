module Ray (
  Point3,
  Direction3,
  Time,
  Ray,
  position_at_time
) where
  
import Vector3

type Point3 = Vector3
type Direction3 = Vector3
type Time = Float
type Ray = (Point3, Direction3) -- base and direction

position_at_time :: Ray -> Time -> Point3
position_at_time (base, dir) t = base `add` (scalarmult dir t)
