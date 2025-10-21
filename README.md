<img src="https://i.imgur.com/2UKMpiy.png">

# M(é)nad(e)\s

An active matrix,

and a language to describe images. Built on top of Haskell.

Takes image seeds and generates new images through transformations,

if you dare to try.

> slow and complex digital drawing.

<img src="https://i.imgur.com/SLxFSDj.png">

## Prerequisites

Install stack->haskell if you haven't already,

[Ref](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

Install the haskell library gloss and its dependencies:

`stack install gloss`


[More info](https://hackage.haskell.org/package/gloss)

Read about the history of technology and its relation to "humanness".

## Compile with stack

`stack ghc -- Main.hs`

## Draw

`./Main`

## More info

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


# Detailed installation instructions thanks to [marto-xyz](https://github.com/marto-xyz) !
## Instalar Stack (el sistema de compilación de Haskell)
Descargado desde: 👉 https://docs.haskellstack.org/en/stable/install_and_upgrade/

Luego confirmamos que funcione con:
`stack --version`

## Descargar el proyecto Menades
Desde GitHub: 👉 https://github.com/karen-pal/menades
Se puede descargar el zip, o hacer git clone. 

Si es zip, lo descomprimimos en:
`\PATH\TO\menades-master`

En windows
`C:\PATH\TO\menades-master`

## Verificamos dependencias

El proyecto depende de:

- Gloss (para gráficos)
- GLUT (librería de ventanas de OpenGL)
- BMP files (imágenes que Menades usa para dibujar)

Instalamos Gloss con:

`stack install gloss`

## Intentar ejecutar directamente

Dentro de la carpeta del proyecto:

`stack ghc -- Main.hs`
En linux:
`./Main`
En windows:
`Main.exe`

## Troubleshooting
### Error unknown GLUT entry glutInit en windows
Si el programa abrió el menú:
> ¿CON CUANTOS ARCHIVOS QUERES DIBUJAR? (1 O 2)
pero al intentar abrir la ventana gráfica dio:

`user error (unknown GLUT entry glutInit)`

Intentamos reparar con FreeGLUT

Descargamos FreeGLUT desde:👉 https://www.transmissionzero.co.uk/software/freeglut-devel/

En windows descomprimimos y copiamos: `freeglut.dll`


en la misma carpeta que el ejecutable:

`C:\PATH\TO\menades-master`


Luego recompilamos:

`stack ghc -- Main.hs`


Pero el error persistió:

`user error (unknown GLUT entry glutInit)`

Para esto copiar los archivos de las carpetas x64 (bin, lib) por las dudas, también copié los de la carpeta "INCLUDE"

Special thanks to [@marto-xyz](https://github.com/marto-xyz) for contributing these instructions and for the windows error solution.
