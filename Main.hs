module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Display
import Graphics.UI.GLUT.Begin
import Dibujo
import Interp
import Data.List
import System.Directory
import qualified Basico.Escher as E
import qualified Basico.FromFile as F

{-
librerias para que eventualmente se pueda exportar a PNG:
import Graphics.Gloss.Export.PNG
import ToFile
-}

data Conf a = Conf {
    basic :: Output a
  , fig  :: Dibujo a
  , width :: Float
  , height :: Float
  }

ej x y = Conf {
                basic = E.interpBas
              , fig = E.ejemplo
              , width = x
              , height = y
              }

-- Dada una computación que construye una configuración, mostramos por
-- pantalla la figura de la misma de acuerdo a la interpretación para
-- las figuras básicas. Permitimos una computación para poder leer
-- archivos, tomar argumentos, etc.
initial :: IO (Conf a) -> IO ()
initial cf = cf >>= \cfg ->
                  let x  = width cfg
                      y  = height cfg
                  in display win black $ interp (basic cfg) (fig cfg) (0,0) (x,0) (0,y)
                  --in display win white . withGrid $ trian1 (0, 0) (x, 0) (0, y)
  --where withGrid p = pictures [p, color grey $ grid 25 (0,0) 100 10]
  --      grey = makeColorI 120 120 120 120

win = InWindow "M(é)nad(e)\\s" (1280, 700) (0, 0)
--main antes de hacerlo que tome archivos como seed era el siguiente:
--main = initial $ return (ej 100 100)

--ahora el main es este:
{- comentarios generales sobre las imágenes seed
como verás, las seeds tienen que estar en ./img
tienen que ser bmp, esto nosotras lo hacemos creando pngs(para que tenga
transparencias). Luego usamos el comando convert para pasarlo a bmp.
La tupla antes del else en la linea 58 son las dimensiones de la imagen seed...
-}
main = do files <- getDirectoryContents "./img"
          let filtered = filter (isSuffixOf ".bmp") files
          {-
          -- modo simple
          putStrLn "DIBUJOS DISPONIBLES:"
          mapM_ putStrLn filtered
          putStr "-> Elegir dibujo: "
          dibujo <- getLine
          if elem dibujo filtered then initial $ imgFromFile ("img/" ++ dibujo) (300, 300) else main
          --imgFromFile ("img/" ++ dibujo) (dim1, dim2) vendría a ser la picture que querés guardar en un archivo
          -}
        --lo siguiente es código muerto/intentos de cosas
        --ignorar


        --intento de exportar el resultado a un archivo png (>:{)
          {-exportPictureToPNG (300,300) transp "./img/guardando" pict
            where pict = imgFromFile ("img/" ++ dibujo) (300, 300)
                  transp = makeColor 0 0 0 1
          -}

        --las superpone solamente (por ahora)
          putStrLn "¿CON CUANTOS ARCHIVOS QUERES DIBUJAR? (1 O 2)"
          option <- getLine
          if option == "1" then do
              putStrLn "DIBUJOS DISPONIBLES:"
              mapM_ putStrLn filtered
              putStr "-> Elegir dibujo: "
              dibujo <- getLine
              if elem dibujo filtered then initial $ imgFromFile ("img/" ++ dibujo) (30, 30) else main
          else do
               putStrLn "DIBUJOS DISPONIBLES:"
               mapM_ putStrLn filtered
               putStr "-> Elegir dibujo1: "
               dibujo1 <- getLine
               putStr "-> Elegir dibujo 2: "
               dibujo2 <- getLine
               if (elem dibujo1 filtered) && (elem dibujo2 filtered) then
                    initial $ imgFromFile' ("img/" ++ dibujo1) ("img/" ++ dibujo2) (300, 300) else main


imgFromFile :: String -> Vector -> IO (Conf F.File)
imgFromFile s (x, y) = do
                    img <- loadBMP s
                    return $ Conf {
                                    basic  = F.interpBas img (x, y)
                                  -- interpBas img (x, y) devuelve una función que al interpretar una básica
                                  -- devuelve img si es la del archivo o otra si no lo es.
                                  , fig    = F.ejemplo
                                  , width  = x
                                  , height = y
                                  }

--el siguiente imgFromFile es para hacer que tome dos imágenes como seed
imgFromFile' :: String -> String -> Vector -> IO (Conf F.File)
imgFromFile' s1 s2 (x, y) = do
                    img1 <- loadBMP s1
                    img2 <- loadBMP s2
                    return $ Conf {
                                    basic  = F.interpBas (Interp.union img1 img2) (x, y)
                                  -- interpBas img (x, y) devuelve una función que al interpretar una básica
                                  -- devuelve img si es la del archivo o otra si no lo es.
                                  , fig    = F.ejemplo
                                  , width  = x
                                  , height = y
                                  }
