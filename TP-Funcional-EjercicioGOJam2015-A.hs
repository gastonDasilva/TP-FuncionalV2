import Prelude
import Data.Char
import qualified Data.Text as T

save :: String-> IO() 
save x = writeFile "datosOut.txt" x

load :: (Read a) => FilePath -> IO a 
load f = do
           s <- readFile f
           return (read s)

separarPorCasosEntersFold:: [Char]->[[Char]]
separarPorCasosEntersFold cs = foldr (\x r -> z x r) [] cs
                         where z '\n' r = []:r
                               z c r = concatenarHead c r  

dameTuMaximoDeTimidez:: [Char]->[Char]
dameTuMaximoDeTimidez cs = foldr (\c r -> if (  c /= ' ')then c:r else [] ) [] cs

  
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
agregarAmigoSiEsNecesario cs n  = if (esNecesarioAgregarAmigos cs )then agregarAmigoSiEsNecesario (agregarAmigo  cs (cantCharsForTimidez cs) ) (n+1) else n 

agregarAmigo::[Char]-> Int ->[Char]
--Agrega un amigo de SI =0 siempre Y va sumando el SI si ya no se le puede agregar mas amigos es decir el c == '9'
agregarAmigo [] n = [] 
agregarAmigo (c:cs) 0 = if ( c == '9')then c:(agregarAmigo cs 0) else (intToDigit ((digitToInt c) +1)): cs
agregarAmigo (c:cs) n = c: (agregarAmigo cs (n-1)) 




sacarTuMaximoDeTimidez:: [Char] -> [Char]
sacarTuMaximoDeTimidez cs = drop (cantCharsForTimidez cs) cs 

cantCharsForTimidez:: [Char] -> Int
cantCharsForTimidez [] = 0
cantCharsForTimidez (c:cs) = if (c /= ' ') then 1+ (cantCharsForTimidez cs) else 1 

esNecesarioAgregarAmigos:: [Char]-> Bool
esNecesarioAgregarAmigos cs =  h (sacarTuMaximoDeTimidez cs)  ( dameTuMaximoDeTimidez cs) 
                               where h cs c = (stringToInt c) > (contarCuantaGenteSeLevanta cs 0 0) 

contarCuantaGenteSeLevanta::[Char]-> Int -> Int -> Int -- z son la cantidad de personas con nivel de timidez -1
contarCuantaGenteSeLevanta [] n z = z
contarCuantaGenteSeLevanta (c:cs) n z =  h z n (digitToInt c) (contarCuantaGenteSeLevanta cs (n+1) ((digitToInt c) + z) ) (contarCuantaGenteSeLevanta cs (n+1)  z )
                                          where h z n nc r1 r2= if (z >= n) then r1 else r2

gestionarEscrituraDeLTest::(Int,Int) -> String
gestionarEscrituraDeLTest (x, y) = "Case #"++ (intToString x) ++ ": " ++(intToString y)
                                     
gestionarLaEsctiruraAllTestFold:: [(Int,Int)] -> String
gestionarLaEsctiruraAllTestFold xs = foldr(\x r -> (gestionarEscrituraDeLTest x) ++('\n':r ) ) [] xs


stringToText:: String -> T.Text
stringToText s = T.pack s


intToString:: Int->String
intToString n =  show n 

stringToInt::String ->Int
stringToInt s = read s ::Int


cargarDatos::  FilePath ->IO() 
cargarDatos f = do 
                     x <- readFile f
                     xsSep <- fmap separarPorCasosEntersFold (return x)
                     xsRes <- fmap cantDeAmigosPorTest (return xsSep)
                     xsResEsc <- fmap gestionarLaEsctiruraAllTestFold (return xsRes)
                     save xsResEsc
                     --print x 
                     --print xsRes
                     --print xsResEsc 


ver:: FilePath ->IO()
ver f = do
          x <- readFile f
          print x
                     


                               