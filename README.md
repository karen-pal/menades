<img src="https://i.imgur.com/2UKMpiy.png">

# M(é)nad(e)\s

An active matrix,

and a language to describe images. Built on top of Haskell.

Takes image seeds and generates new images through transformations,

if you dare to try.

> slow and complex digital drawing.

<img src="https://i.imgur.com/SLxFSDj.png">

## Prerequisites

Install haskell,

Install the haskell library gloss and its dependencies,

Read about the history of technology and its relation to "humanness".

## Compile with stack

`stack ghc -- Main.hs`

## Draw

`./Main`

## Usage

### Seed details
Seeds have to be bmp files. In Linux, you can use the `imagemagick` tool to convert a png to bmp using the command
`convert <img-name>.png output.bmp`

> bmp files generated in Windows are known to have encoding problems.

### Hyperparameters

I'll give you a few clues:
* transformation currently being drawn is ejemplo in `Basica/FromFile.hs`

* dimensions of canvas in which She draws. `Main.hs:85`

* background color in initial config: `Main.hs:40`

> Forgive dead code, I have no burial ceremonies put in place...

## More Information
To read more about this project, [consult these slides](https://docs.google.com/presentation/d/13Qb4pv03XYT_mUEkGlTB7VZeENPSqXbDxNuKmmtyvZA/edit?usp=sharing)
