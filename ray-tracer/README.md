# haskell ray tracer

## This is not a ray marcher, and it's not the final product!
This raytracer is purely for research purposes. It renders a scene, consisting of a list of objects. Every object has a function that takes a point in space and returns Material, which is the specific color of the pixel. This list of colors is then applied to a function alongside the width and height of the desired image. The result is a string representation of that list, written to a PPM formatted file.

## Original author:
http://www.nobugs.org/developer/htrace/

## Added features:
- Source is now broken up into different files.
- Scene is created in a separate binary file, which is later read from.
- The PPM output is converted to the lighter PNG format.

## Setup guide:

### Software requirements:
- ghc
- cabal

### Installing needed libraries:
```sh
$ cabal install ppm
$ cabal install hip
```

### How to build:
```sh
$ ghc -O2 htrace.hs
```

### How to run:
Edit `scene_creator.hs` beforehand to create your scene. After you have your scene do:

```sh
$ runghc scene_creator.hs
$ ./htrace
```
