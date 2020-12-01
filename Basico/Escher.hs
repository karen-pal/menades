module Basico.Escher where
import Dibujo
import Interp

data Escher = Triang | FShape | Rectang | Vacía
            deriving (Show,Eq)

interpBas :: Output Escher  --interpBas es como un diccionario
interpBas Triang = trian1   --acá vas a tener las instancias, dado un nombre de una Escher, llamás a las funciones
interpBas FShape = fShape   --correspondientes que las interpretan/manipulan
interpBas Rectang = rectan
interpBas Vacía = vacia

--en ejemplo vas a tener la que vas a querés dibujar. es para pasarle el parámetro al main
ejemplo :: Dibujo Escher 
ejemplo = escher 3 $ Simple Rectang
-- el siguiente es un resultado más interesante 
-- ejemplo = (comp (escher 2) 3) $ Simple Rectang



-- el dibujo u
dibujo_u :: Dibujo Escher -> Dibujo Escher
dibujo_u = encimar4 . Rot45

-- el dibujo t
dibujo_t :: Dibujo Escher -> Dibujo Escher
dibujo_t d = Encimar d (Encimar (Rot45 d) (r270 (Rot45 d)))


-- esquina con nivel de detalle en base a la figura d
esquina :: Int -> Dibujo Escher -> Dibujo Escher
esquina 0 d = Simple Vacía
esquina n d = cuarteto (esquina (n-1) d) (lado (n-1) d) (Rotar (lado (n-1) d)) (dibujo_u d)


-- lado con nivel de detalle
lado :: Int -> Dibujo Escher -> Dibujo Escher
lado 0 d = Simple Vacía
lado n d = cuarteto (lado (n-1) d) (lado (n-1) d) (Rotar d) d


-- por suerte no tenemos que poner el tipo!
noneto p q r s t u v w x = Apilar 1 2 (fila p q r) $ Apilar 1 1 (fila s t u) (fila v w x) 
                           where fila a b c = Juntar 1 2 a $ Juntar 1 1 b c

-- el dibujo de Escher:
escher :: Int -> Dibujo Escher -> Dibujo Escher
escher n e = noneto p q r s t u v w x
             where d = e
                   p = esquina n d
                   q = lado n d
                   r = r270 $ p
                   s = Rotar q
                   t = dibujo_u d
                   u = r270 q
                   v = Rotar p
                   w = r180 q
                   x = r180 p


esquina' :: Int -> Dibujo Escher -> Dibujo Escher
esquina' 0 d = Simple Vacía
esquina' n d = cuarteto (esquina (n-1) d) (lado (n-1) d) (Rot45 (lado (n-1) d)) (dibujo_u d)

noneto' p q r s t u v w x = Apilar 1 2 (fila p q r) $ Apilar 1 1 (fila s t u) (fila v w x) 
                           where fila a b c = Juntar 1 2 a $ Juntar 1 1 b c 

--un escher rancio para jugar con asimetrías
escher' :: Int -> Dibujo Escher -> Dibujo Escher
escher' n e = noneto' p q r s t u v w x
             where d = e
                   p = Rot45 $ esquina' n d
                   q = lado n d
                   r = esquina' n p
                   s = Encimar (Rotar q) q
                   t = dibujo_u d
                   u = r270 q
                   v = Rotar $ esquina' n p
                   w = r180 q
                   x = esquina' n d

escher'' :: Int -> Dibujo Escher -> Dibujo Escher
escher'' n e = noneto' p q r s t u v w x
             where d = e
                   p = Rot45 $ esquina' n d
                   q = esquina' n d
                   r = esquina' n p
                   s = Rotar q
                   t = dibujo_u d
                   u = r270 q
                   v = Rotar $ esquina' n p
                   w = r180 q
                   x = esquina' n d

--funciones para truchet...siguiendo el paper: http://archive.bridgesmathart.org/2018/bridges2018-39.pdf

confg_truchet1 p q = Apilar 1 4 (fila p q) $ Apilar 1 3 (fila q p) $ Apilar 1 2 (fila p q) $ Apilar 1 1 (fila q p) (fila p q) 
                  where fila a b =Juntar 1 4 a $ Juntar 1 3 b $ Juntar 1 2 a $ Juntar 1 1 b a

truchet1 :: Dibujo Escher -> Dibujo Escher
truchet1 d = confg_truchet1 d (cuarteto1 d)

confg_truchet2 p q = Apilar 1 3 (fila p q) $ Apilar 1 2 (fila (Espejar q) p) $ Apilar 1 1 (fila p q) (fila (Espejar q) p) 
                  where fila a b = Juntar 1 3 a $ Juntar 1 2 b $ Juntar 1 1 a b

truchet2 :: Dibujo Escher -> Dibujo Escher
truchet2 d = confg_truchet2 d inside_config_t2
            where inside_config_t2 = cuarteto d (cuarteto1 d) (cuarteto1 d) d
