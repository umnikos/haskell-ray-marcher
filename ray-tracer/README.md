# haskell ray tracer

## This is not a ray marcher, and it's not the final product!
This raytracer is purely for research purposes. It renders a scene, consisting of a list of objects. Every object has a function that takes a point in space and returns Material, which is the specific color of the pixel. This list of colors is then applied to a function alongside the width and height of the desired image. The result is a string representation of that list, written to a PPM formatted file.

## Original author:
http://www.nobugs.org/developer/htrace/

## Added features:
- Scene is created in a separate binary file, which is later read from.
- The PPM output is converted to the lighter PNG format.


