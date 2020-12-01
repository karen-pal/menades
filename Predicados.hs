module Predicados where
import Graphics.Gloss
import Dibujo
import Interp
import Basico.Ejemplo

type Pred a = a -> Bool


-- Suponiendo que `v` sea una figura vacía, borrar aquellas figuras que cumplen con el predicado.
limpia :: Pred a -> a -> Dibujo a -> Dibujo a
limpia p v d = mapDib fp d
               where fp a = if p a then v else a 

-- Alguna hoja del árbol cumple con el predicado.
anyDib :: Pred a -> Dibujo a -> Bool
anyDib p = sem p id id id f f (||)
           where f _ _ = (||)

-- Todas las hojas del árbol cumplen con el predicado.
allDib :: Pred a -> Dibujo a -> Bool
allDib p = sem p id id id f f (&&)
           where f _ _ = (&&)

-- Devuelve una descripción textual del árbol sintáctico.
desc :: (a -> String) -> Dibujo a -> String
desc f = sem f rot esp r45 api jun enc
         where rot s         = "rot (" ++ s ++ ")"
               esp s         = "esp (" ++ s ++ ")"
               r45 s         = "r45 (" ++ s ++ ")"
               api n m s1 s2 = "api (" ++ (show n) ++ ":" ++ (show m) ++ ", " ++ s1 ++ ", " ++ s2 ++ ")"
               jun n m s1 s2 = "jun (" ++ (show n) ++ ":" ++ (show m) ++ ", " ++ s1 ++ ", " ++ s2 ++ ")"
               enc s1 s2     = "enc (" ++ s1 ++ ", " ++ s2 ++ ")"
--ejemplos de funciones que toma desc:
--f Rectang = "Rectang"
--f Triang = "Triang"


-- junta todas las figuras básicas de un dibujo
every :: Dibujo a -> [a]
every = sem f id id id g g (++)
          where f x = [x]
                g _ _ = (++)

-- cuenta la cantidad de veces que aparecen las básicas en una 
-- figura.
contar :: Eq a => Dibujo a -> [(a,Int)]
contar d = (sem f id id id g g sumarListaDeTuplas d) []
        where f x = [(x,1)]
              g _ _ = sumarListaDeTuplas

sumarListaDeTuplas :: Eq a => [(a, Int)] -> [(a, Int)] -> [(a, Int)]
sumarListaDeTuplas [] xs = xs
sumarListaDeTuplas xs [] = xs
sumarListaDeTuplas ((x,i):xs) ((y,l):ys) | x==y = (x,i+l):(sumarListaDeTuplas xs ys)
                                         | otherwise = sumarListaDeTuplas xs $ (y,l) : (sumarListaDeTuplas [(x,i)] ys)

{-
contar d = agregarAListaTuplas xs []
          where xs = every d

agregarAListaTuplas :: Eq a => [a] -> [(a,Int)]-> [(a,Int)]
agregarAListaTuplas [] xs = xs
agregarAListaTuplas (y:ys) [] = agregarAListaTuplas ys [(y,1)]
agregarAListaTuplas (y:ys) ((x,i):xs) | y==x = agregarAListaTuplas ys ((x, i+1):xs)
                                      | otherwise = agregarAListaTuplas ys ((x, i) : (agregarAListaTuplas [y] xs))


-}

-- hay 4 rotaciones seguidas (empezando en el tope)
esRot360 :: Pred (Dibujo a)
esRot360 (Rotar (Rotar (Rotar (Rotar d)))) = True
esRot360 _ = False

-- hay 2 espejados seguidos (empezando en el tope)
esFlip2 :: Pred (Dibujo a)
esFlip2 (Espejar (Espejar d)) = True
esFlip2 _ = False

-- la cadena que se toma como parámetro es la descripción
-- del error.
check :: Pred (Dibujo a) -> String -> Dibujo a -> Either String (Dibujo a)
check fun_error s_error d = if fun_error d then Left s_error
                            else Right d

-- aplica todos los chequeos y acumula todos los errores,
-- sólo devuelve la figura si no hubo ningún error.
todoBien' :: [(Pred (Dibujo a), String)] -> Dibujo a -> Either [String] (Dibujo a)
todoBien' [] d = Right d
todoBien' ((f,s):fs) d | f d = case todoBien' fs d of
                                    Left xs -> Left (s:xs)
                                    Right _ -> Left [s]
                       | otherwise = todoBien' fs d  

todoBien = todoBien' [(esRot360, "Rotacion 360"), (esFlip2,"Doble espejado")]

--Definir funciones que corrigen los errores detectados:
noRot360 :: Dibujo a -> Dibujo a
noRot360 (Rotar(Rotar(Rotar(Rotar d)))) = noRot360 d
noRot360 d = d


noFlip2  :: Dibujo a -> Dibujo a
noFlip2 (Espejar(Espejar d)) = noFlip2 d
noFlip2 d = d

-- deben satisfacer 
-- (1) check esRot360 "foo" (noRot360 f) = Right f', para alguna f'
-- (2) check esFlip2 "foo" (noFlip2 f) = Right f', para alguna f'