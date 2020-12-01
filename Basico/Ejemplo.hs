module Basico.Ejemplo where
import Dibujo
import Interp

data Basica = Triang | Rectang | Vacía 
        deriving (Show,Eq)--acá tenés las figuras básicas posibles de manipular, mostrar, etc.
ejemplo :: Dibujo Basica --acá vas a tener la que vas a querés dibujar. es para pasarle el parámetro al main
-- ejemplo = Juntar 1 1 (Simple Triang) (Simple Rectang)

interpBas :: Output Basica --interpBas es como un diccionario
interpBas Triang = trian1   --acá vas a tener las instancias, dado un nombre de una Basica, llamás a las funciones
interpBas Rectang = rectan  --correspondientes que las interpretan/manipulan
interpBas Vacía = vacia
--interpBas d = rotate 90 (interpBas d) 

f :: Basica -> Basica
f Triang = Rectang
f Rectang = Triang


--ejemplo = (Juntar 1 1 (Simple f) (Simple id)) <*> (Simple Triang)
ejemplo = (Juntar 1 1 (Simple f) (Simple id)) <*> (Apilar 1 1 (Simple Rectang) (Simple Triang))
-- ejemplo = (Juntar 1 1 (Simple f) (Simple g)) <*> (Apilar 1 1 (Simple FShape) (Simple TrianD)))


cuartetoId = cuarteto1 $ Simple id

--ejemplo = cuartetoId <*> (Simple Triang)
--ejemplo = cuartetoId <*> (cuartetoId <*> (Simple Triang))
-- es equivalente a
-- ejemplo = (comp (cuartetoId <*>) 2) (Simple Triang)
-- y se puede anidar más!!
--ejemplo = (comp (cuartetoId <*>) 6) (Simple Triang)

simplId = Simple id
cuartetoRotado = (simplId .-. (Rotar simplId)) /// ((r270 simplId) .-. (r180 simplId))
-- ejemplo = cuartetoRotado <*> (Simple Triang)

trebol = cuartetoRotado <*> (Simple Triang) --es un dibujo

--ejemplo = trebol
--ejemplo = (cuarteto1 simplId) <*> trebol --USO: "instruccion" de cómo dibujar una figura <*> figura

hueco = Simple (\x -> Vacía)

arribaIzq = (simplId .-. hueco) /// (hueco .-. hueco)

--ejemplo = arribaIzq <*> trebol --tiene que ser <*> porque arribaIzq es "arbol de funciones" -> "dibujo de funciones"
-- ejemplo = (comp (cuartetoRotado <*>) 7) trebol
