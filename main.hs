import System.IO
import Data.List
import Data.Char


--para executar: ghc trabalho.hs && ./trabalho
imprimir [] = putStr "\n"

imprimir ((l,s):xs) = do putStr(show l ++ " - ")
                         putStr (s ++ "\n")
                         imprimir xs

minusculo [] = []
minusculo (x:xs) = toLower x: minusculo xs

aux n [] = []
aux n (x:xs) = (n, x): aux (n+1) xs

--separar o texto em linhas e a numeralas
numLines xs = aux 1 (lines xs)
--podia colocar assim: numLines xs = zip [1..] xs

--separar cada palavra e dizer sua linha
allNumWords' n [] = []
allNumWords' n (x:xs) = (n, x): allNumWords' n xs

allNumWords [] = []
allNumWords ((n,s):xs) = allNumWords' n (words s) ++ allNumWords xs

--ordenar alfabeticamente as palavras
inverte [] = []
inverte ((a, b):xs) = (b, a): inverte xs

sortByStr xs = sort xs;

--Juntar as ocorrencias das palavras
ocorrencias [] = []
ocorrencias ((x,y):xs) = ([a | (a,b) <- (x,y):xs, b == y], y) : ocorrencias [(a,b) | (a,b) <- (x,y):xs, b/= y]

-- Eliminar as repetições de um mesmo número de linha
elimina [x] = [x]
elimina (x:xs) |x /= head xs = x:elimina xs
               |otherwise = elimina xs

eliminarRep [] = []
eliminarRep ((l,s):xs) = (elimina l, s): eliminarRep xs


main = do putStr "Arquivo: "
          hFlush stdout
          arq <- getLine
          txt <- readFile arq
          let minusculos = minusculo txt
          let linhas = numLines minusculos
          let palavras = allNumWords linhas
          let flip = inverte palavras
          let sort = sortByStr flip
          let sort' = inverte sort
          let ocorrencia = ocorrencias sort'
          let eliminaRep = eliminarRep ocorrencia
          imprimir eliminaRep