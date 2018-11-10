import Prelude

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


--DeterminarCantidadDeAmigosNecesarios: [Char]-> Int
--DeterminarCantidadDeAmigosNecesarios



gestionarCasos::  FilePath ->IO() 
gestionarCasos f = do 
                     x <- readFile f
                     xs <- fmap sacarEntersAndEspaciosFold (return x)
                     xsSep <- fmap separarPorCasosFold (return xs)
                     save xsSep 
                     print xsSep 

main = do 
       x <- readFile "sarasa.txt"
       xs <- fmap sacarEntersAndEspaciosFold (return x)
       save xs
       print xs 


                               