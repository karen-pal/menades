module Basico.FromFile where
import Dibujo
import Interp
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import qualified Basico.Escher as E

data File = Imagen | Vacía
          deriving (Show, Eq)

interpBas :: Picture -> Vector -> File -> FloatingPic
interpBas img (x,y) Imagen = transf f img (x,y)
                            where f img _ = img
interpBas _ _ Vacía        = simple $ pictures []


-- lo siguiente es el interpBas para que tome dos imágenes como seed!
interpBas' :: Picture -> Picture -> Vector -> File -> File -> FloatingPic
interpBas' img1 img2 (x,y) Imagen Imagen = transf f (union img1 img2) (x,y)
                                                    where f img _ = img
interpBas' _ _ _ Vacía Vacía             = simple $ pictures []

escherAFile :: E.Escher -> File
escherAFile E.Vacía = Vacía
escherAFile _       = Imagen

--ejemplo trivial
--ejemplo = Simple Imágen

--ejemplo que se dibuja posta:
ejemplo = mapDib escherAFile esch
        where esch = E.escher' 1 $ E.truchet1 $ Simple E.Triang
        	--lo siguiente son configuraciones que probé y me fueron gustando
            -- E.escher 4 $ Simple E.Triang
            -- E.escher' 1 $ E.truchet2 $ E.escher 1  $ Simple E.Triang

            --E.escher 1 $ E.truchet2 $ E.truchet1 $ Simple E.Triang <--hermoso pero costoso, como todas las cosas buenas
            --E.truchet2 $ E.escher 2 $ Simple E.Triang
            -- E.escher 3 $ E.truchet2 $ Simple E.Triang <--genial
            --E.truchet2 $ E.truchet2 $ Simple E.Triang
            --
            --E.truchet2 $ Simple E.Triang
            --E.escher' 1 $ E.escher 4 $ Simple E.Triang
            --Encimar (E.escher 2 $ Simple E.Triang) (E.escher' 2 $ Simple E.Triang)
            --E.escher 1 $ E.escher 3 $ Juntar 1 2 (Simple E.Triang) (Simple E.Triang)
            --E.dibujo_u $ (E.escher 3 $ Simple E.Triang)
--Encimar (Simple Imágen) (Rot45 $ Simple Imágen)

