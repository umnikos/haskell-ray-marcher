{-# LANGUAGE DeriveGeneric #-}

module SceneCreator(
  BShape (..),
) where

import  Data.Binary
import  GHC.Generics (Generic)
import  Vector3
import  Ray
import  Shapes
import  Color

data BShape = BSphere Point3 Radius (Color, Float, Float)
           | BPlane Normal Float (Color, Float, Float)
           deriving (Generic)

instance Binary BShape
