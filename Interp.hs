module Interp where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo
type FloatingPic = Vector -> Vector -> Vector -> Picture
type Output a = a -> FloatingPic

-- el vector nulo
zero :: Vector
zero = (0,0)

half :: Vector -> Vector
half = (0.5 V.*)

opposite :: Vector -> Vector
opposite = (zero V.-)

hlines :: Vector -> Float -> Float -> [Picture]
hlines v@(x,y) mag sep = map (hline . (*sep)) [0..]
  where hline h = line [(x,y+h),(x+mag,y+h)] 

-- Una grilla de n líneas, comenzando en v con una separación de sep y
-- una longitud de l (usamos composición para no aplicar este
-- argumento)
grid :: Int -> Vector -> Float -> Float -> Picture
grid n v sep l = pictures [ls,translate 0 (l*toEnum n) (rotate 90 ls)]
  where ls = pictures $ take (n+1) $ hlines v sep l

-- figuras adaptables comunes
trian1 :: FloatingPic
trian1 a b c = line $ map (a V.+) [zero, half b V.+ c , b , zero]

trian2 :: FloatingPic
trian2 a b c = line $ map (a V.+) [zero, c, b,zero]

trianD :: FloatingPic
trianD a b c = line $ map (a V.+) [c, half b , b V.+ c , c]

rectan :: FloatingPic
rectan a b c = line [a, a V.+ b, a V.+ b V.+ c, a V.+ c,a]

simple :: Picture -> FloatingPic
simple p _ _ _ = p

vacia = simple $ pictures []

fShape :: FloatingPic
fShape a b c = line . map (a V.+) $ [ zero,uX, p13, p33, p33 V.+ uY , p13 V.+ uY 
                 , uX V.+ 4 V.* uY ,uX V.+ 5 V.* uY, x4 V.+ y5
                 , x4 V.+ 6 V.* uY, 6 V.* uY, zero]    
  where p33 = 3 V.* (uX V.+ uY)
        p13 = uX V.+ 3 V.* uY
        x4 = 4 V.* uX
        y5 = 5 V.* uY
        uX = (1/6) V.* b
        uY = (1/6) V.* c

-- Dada una función que produce una figura a partir de un a y un vector
-- producimos una figura flotante aplicando las transformaciones
-- necesarias. Útil si queremos usar figuras que vienen de archivos bmp.
transf :: (a -> Vector -> Picture) -> a -> Vector -> FloatingPic
transf f d (xs,ys) a b c  = translate (fst a') (snd a') .
                             scale (magV b/xs) (magV c/ys) .
                             rotate (0 - ang) $ f d (xs,ys)
  where ang = radToDeg $ argV b
        a' = a V.+ half (b V.+ c)

--transf' f d1 d2 (x,y) = transf f (union d1 d2) (x,y)

union :: Picture -> Picture -> Picture
union p1 p2 = pictures [p1, p2]

interp :: Output a -> Output (Dibujo a)
interp f (Simple x) a b c = f x a b c
interp f (Rotar d) a b c = fig (a V.+ b) c (opposite b)
                           where fig = interp f d
interp f (Espejar d) a b c = (interp f d) (a V.+ b) (opposite b) c
interp f (Rot45 d) a b c = (interp f d) (a V.+ half (b V.+ c)) (half (b V.+ c)) (half (c V.- b))
interp f (Encimar d1 d2) a b c = ((interp f d1) a b c) `union` ((interp f d2) a b c)
interp f (Juntar x y d1 d2) a b c = ((interp f d1) a b' c) `union` ((interp f d2) (a V.+ b') (r' V.* b) c)
                                    where r' = (fromIntegral y) /(fromIntegral (x + y))
                                          r = (fromIntegral x) /(fromIntegral (x + y))
                                          b' = r V.* b 
interp f (Apilar x y d1 d2) a b c = ((interp f d1) (a V.+ c') b (r V.* c)) `union` ((interp f d2) a b c')
                                    where r' = (fromIntegral y) /(fromIntegral (x + y))
                                          r = (fromIntegral x) /(fromIntegral (x + y))
                                          c' = r' V.* c


{-
interp es el interprete -> interpBas es el diccionario y ejemplo es la figura a dibujar.
Lo podemos ver en lo siguiente: 

fig = interp interpBas (Simple Triang)
    = interpBas Triang
    = trian1

y con los vectores a b c

PP:: FLoatingPic
PP = interp interpBas (Rotar (Simple Triang))
PP a b c = trian1 (a + b) c (-b)


eval (Abs (Val Tres))

evalNum Tres = 3
-}