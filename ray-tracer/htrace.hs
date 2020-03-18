import Rendering
import SceneCreator
import ConvertPPMToPng

main = do
  writeFile "test.ppm" (render_to_pgm 500 500)
  convertPPMToPng
