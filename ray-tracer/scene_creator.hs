import SceneCreator
import Material
import Data.Binary

shapes :: [BShape]
shapes = [ BPlane (0,(-1),0) 50 shinyred,
     BSphere (50,10,100) 40 semishinygreen,
     BSphere (350,10,100) 40 semishinygreen,
     BSphere (-80,0,80) 50 flatred]

main = encodeFile "scene" shapes
