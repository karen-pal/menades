module Dibujo where

-- definir el lenguaje
data Dibujo a = Simple a | Rotar (Dibujo a)| Espejar (Dibujo a) | Rot45 (Dibujo a) 
              | Apilar Int Int (Dibujo a) (Dibujo a)
              | Juntar Int Int (Dibujo a) (Dibujo a)
              | Encimar (Dibujo a) (Dibujo a)
          deriving (Show, Eq)

-- composición n-veces de una función con sí misma.
comp :: (a -> a) -> Int -> a -> a
comp f 0 = id
comp f n = f . comp f (n-1)

-- rotaciones de múltiplos de 90.
r180 :: Dibujo a -> Dibujo a
r270 :: Dibujo a -> Dibujo a

r180 = comp Rotar 2
r270 = comp Rotar 3

-- Pone una figura sobre la otra, ambas ocupan el mismo espacio
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) = Apilar 1 1

-- Pone una figura al lado de la otra, ambas ocupan el mismo espacio
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) = Juntar 1 1

-- Superpone una figura con otra
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) = Encimar

-- dada una figura la repite en cuatro cuadrantes
cuarteto1 :: Dibujo a -> Dibujo a
cuarteto1 d = (d .-. d) /// (d .-. d)

cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto a b c d = (a /// b) .-. (c /// d)
 
-- una figura repetida con las cuatro rotaciones, superimpuestas.
encimar4 :: Dibujo a -> Dibujo a
encimar4 d = d ^^^ (ciclar d)

-- cuadrado con la misma figura rotada $i$ por $90$ para $i \in \{1..3\}$.
-- No confundir con encimar4!
ciclar :: Dibujo a -> Dibujo a
ciclar d = (Rotar d) ^^^ (r180 d) ^^^ (r270 d)

-- ver un a como una figura
pureDibe :: a -> Dibujo a
pureDibe = Simple

-- map para nuestro lenguaje
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib f (Simple a) = Simple (f a)
mapDib f (Rotar d) = Rotar (mapDib f d)
mapDib f (Espejar d) = Espejar (mapDib f d)
mapDib f (Rot45 d) = Rot45 (mapDib f d)
mapDib f (Juntar x y d1 d2) = Juntar x y (mapDib f d1) (mapDib f d2)
mapDib f (Apilar x y d1 d2) = Apilar x y (mapDib f d1) (mapDib f d2)
mapDib f (Encimar d1 d2) = Encimar (mapDib f d1) (mapDib f d2)
-- mapDib f = cambia $ Simple . f

instance Functor Dibujo where
    fmap = mapDib

-- verificar que las operaciones satisfagan
-- 1. mapDib id = id
-- 2. mapDib (g . f) = mapDib g . mapDib f

cambia :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
cambia f (Simple a) = f a
cambia f (Rotar d) = Rotar (cambia f d)
cambia f (Espejar d) = Espejar (cambia f d)
cambia f (Rot45 d) = Rot45 (cambia f d)
cambia f (Apilar x y d1 d2) = Apilar x y (cambia f d1) (cambia f d2)
cambia f (Juntar x y d1 d2) = Juntar x y (cambia f d1) (cambia f d2)
cambia f (Encimar d1 d2) = Encimar (cambia f d1) (cambia f d2)

-- Son equivalentes:
-- mapDib (\Triang -> Rect) dib
-- cambia (\Triang -> (Simple Rect)) dib

-- También se puede hacer: (o sea que cambia es más poderoso que mapDib)
-- cambia (\Triang -> (Rotar (Simple Rect)) dib

-- convencerse que se satisface
-- 1. cambia pureDibe = id
-- 2. cambia f (pureDibe a) = f a
-- 3. (cambia g) (cambia f ma) = cambia ((cambia g) . f) ma
-- 3 es equivalente a `ma >>= f >>= g`

{- el 3 visto con un ejemplo:

g :: Basica -> Dibujo Basica
g x = Rotar (Simple x)

f Triang = Simple Rectang
f Rectang = Simple Triang

cambia (cambia g . f) (Simple Triang)
= (cambia g . f) Triang 
= cambia g $ Simple Rectang
= g Rectang
= Rotar $ Simple Rectang

-}

instance Monad Dibujo where
    (>>=) = flip cambia -- flip simplemente da vuelta los args
    -- (>>=) d f = cambia f d

-- Para que sea Monad tiene que ser Applicative !

instance Applicative Dibujo where
    --(Simple f) <*> (Simple a) = Simple (f a) 
    --(Simple f) <*> (Rotar d) = Rotar (mapDib f d)
    (Simple f) <*> d = mapDib f d
    (Rotar d1) <*> d2 = Rotar (d1 <*> d2)
    (Espejar d1) <*> d2 = Espejar (d1 <*> d2)
    (Rot45 d1) <*> d2 = Rot45 (d1 <*> d2)
    (Apilar x y d1 d2) <*> d3 = Apilar x y (d1 <*> d3) (d2 <*> d3)
    (Juntar x y d1 d2) <*> d3 = Juntar x y (d1 <*> d3) (d2 <*> d3)
    (Encimar d1 d2) <*> d3 = Encimar (d1 <*> d3) (d2 <*> d3)
    
    pure = pureDibe

-- semántica del lenguaje
sem :: (a -> b) -> (b -> b) -> (b -> b) -> (b -> b) ->
       (Int -> Int -> b -> b -> b) ->
       (Int -> Int -> b -> b -> b) ->
       (b -> b -> b) ->
       Dibujo a -> b
sem f _ _ _ _ _ _ (Simple a) = f a
sem f g h i j k l (Rotar d) = g $ sem f g h i j k l d
sem f g h i j k l (Espejar d) = h $ sem f g h i j k l d
sem f g h i j k l (Rot45 d) = i $ sem f g h i j k l d
sem f g h i j k l (Apilar m n d1 d2) = j m n (sem f g h i j k l d1) (sem f g h i j k l d2)
sem f g h i j k l (Juntar m n d1 d2) = k m n (sem f g h i j k l d1) (sem f g h i j k l d2)
sem f g h i j k l (Encimar d1 d2) = l (sem f g h i j k l d1) (sem f g h i j k l d2)
