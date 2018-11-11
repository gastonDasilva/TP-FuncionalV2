import Prelude
import Data.Char
import qualified Data.Text as T

save :: (Show a)=> a -> IO() 
save x = writeFile "datosOut.txt" (show x)

load :: (Read a) => FilePath -> IO a 
load f = do
           s <- readFile f
           return (read s)

sacarEntersAndEspaciosFold:: [Char]-> [Char]
sacarEntersAndEspaciosFold xs = foldr (\x r -> h x r) [] xs
                                where h '\n' r = ' ':r
                                      h ' ' r = r 
                                      h c1 r = c1:r
 

separarPorCasosFold:: [Char]->[[Char]]
separarPorCasosFold cs = foldr (\x r -> z x r) [] cs
                         where z ' ' r = []:r
                               z c r = concatenarHead c r   

concatenarHead:: Char -> [[Char]] -> [[Char]]
concatenarHead c [] = [[c]]
concatenarHead c (cs:css) = (c:cs):css



cantDeAmigosPorTest:: [[Char]]-> [(Int,Int)]
cantDeAmigosPorTest css  = (separarEnTuplasPorTest  (tail css) 1)

separarEnTuplasPorTest:: [[Char]]-> Int-> [(Int,Int)]
separarEnTuplasPorTest [] n = []
separarEnTuplasPorTest (cs:css) n = (n, cantidadDeAmigosNecesarios cs): (separarEnTuplasPorTest css (n+1)) 


cantidadDeAmigosNecesarios:: [Char]->Int
cantidadDeAmigosNecesarios cs = agregarAmigoSiEsNecesario cs  0 -- if (esNecesarioAgregarAmigos cs) then (agregarAmigo cs)

agregarAmigoSiEsNecesario::[Char]->Int ->Int
agregarAmigoSiEsNecesario cs n = if (esNecesarioAgregarAmigos cs) then agregarAmigoSiEsNecesario (agregarAmigo cs 0 ) (n+1) else n 

agregarAmigo::[Char]-> Int ->[Char]
--Agrega un amigo de SI =0 siempre 
agregarAmigo (c:cs) n = if ( n == 0 ) then c:(intToDigit (digitToInt (head cs) +1): (tail cs)) else c:cs 


esNecesarioAgregarAmigos:: [Char]-> Bool
esNecesarioAgregarAmigos cs =  h (tail cs) (head cs)
                               where h cs c = (digitToInt c) > (contarCuantaGenteSeLevanta cs 0 0)


contarCuantaGenteSeLevanta::[Char]-> Int -> Int -> Int -- z son la cantidad de personas con nivel de timidez -1
contarCuantaGenteSeLevanta [] n z = z
contarCuantaGenteSeLevanta (c:cs) n z =  h z n (digitToInt c) (contarCuantaGenteSeLevanta cs (n+1) ((digitToInt c) + z) ) (contarCuantaGenteSeLevanta cs (n+1)  z )
                                          where h z n nc r1 r2= if (z >= n) then r1 else r2

gestionarEscrituraDeLTest::(Int,Int) -> String
gestionarEscrituraDeLTest (x, y) = "Case #"++ (intToString x) ++ ": " ++(intToDigit y):""
                                     
gestionarLaEsctiruraAllTest:: [(Int,Int)] -> String
gestionarLaEsctiruraAllTest [] = []
gestionarLaEsctiruraAllTest (x:xs) = (gestionarEscrituraDeLTest x) ++ ('\n':gestionarLaEsctiruraAllTest xs)

gestionarLaEsctiruraAllTestFold:: [(Int,Int)] -> String
gestionarLaEsctiruraAllTestFold xs = foldr(\x r -> (gestionarEscrituraDeLTest x) ++('\n':r ) ) [] xs


stringToText:: String -> T.Text
stringToText s = T.pack s


intToString:: Int->String
intToString n =  show n 


gestionarCasos::  FilePath ->IO() 
gestionarCasos f = do 
                     x <- readFile f
                     xs <- fmap sacarEntersAndEspaciosFold (return x)
                     xsSep <- fmap separarPorCasosFold (return xs)
                     xsRes <- fmap cantDeAmigosPorTest (return xsSep)
                     xsResEsc <- fmap gestionarLaEsctiruraAllTestFold (return xsRes)
                     --xsText <-  fmap stringToText (return xsResEsc)
                     save xsResEsc
                     print xsSep
                     print xsRes
                     print xsResEsc 
                     

main = do 
       x <- readFile "sarasa.txt"
       xs <- fmap sacarEntersAndEspaciosFold (return x)
       save xs
       print xs 


                               