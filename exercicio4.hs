import System.IO --necessária para função hFlush, que limpa o buffer para a entrada do usuario.

import Data.List -- necessária para import da função sortBy, que ordena a lista de tuplas. e groupBy, que agrupa tuplas com y iguais

import Data.Char -- necessária para import a função isPunctuation, para remover as pontuações do texto
import Data.Ord -- necessária para import da função comparing, que irá comparar os y das tuplas da lista alfabeticamente,
-- (transforma cada letra em um int correspondente ao seu código ASCII, assim, comparando qual é menor ou maior)

type Doc = String
type Line = String
type Word' = String

main = do   putStrLn "Arquivo a ser lido (deve estar no mesmo diretório): "
            hFlush stdout
            arquivoName <- getLine
            texto <- readFile arquivoName
            let textonoPunc = removePunctuation texto
            print (makeindex textonoPunc)
            
removePunctuation [] = [] -- remove as pontuações do texto, que irão interferir na checagem de palavras.
removePunctuation (x:xs) = if isPunctuation x then removePunctuation xs else x : removePunctuation xs

numLines :: [Line] -> [(Int,Line)] --recebe uma lista de strings e retorna uma lista de tuplas (numero da linha, texto da linha)
numLines xs = numLines' 1 xs
numLines' _ [] = []
numLines' n (x:xs) = [(n, x)] ++ numLines' (n+1) xs


allNumWords :: [(Int,Line)] -> [(Int,Word')] -- recebe uma lista de tuplas (numero da linha, texto da linha) e separa em uma lista de tuplas (numero da linha, palavra)
-- a separação por palavras ocorre com a função words.
allNumWords ([]) = []
allNumWords ((x,y):xs) = allNumWords' x y ++ allNumWords xs
allNumWords' n xs = [(n, word) | word <- words xs]


sortLs :: [(Int,Word')] -> [(Int,Word')] -- A função sortLS recebe uma lista de tuplas (inteiro, string) e retorna o mesmo tipo ordenado por ordem alfabética.
sortLs = sortBy (comparing snd) -- Ordena a lista de tuplas usando a segunda componente (snd) de cada tupla, y. Funciona comparando códigos ASCII


almalgamate :: [(Int,Word')] -> [([Int],Word')] -- A função almalgamate recebe uma lista de tuplas (numero da linha,palavra) e retorna uma lista de tuplas ([linhas em que a palavra ocorre], palavra)
almalgamate xs = map combineGroups (groupBy (\x y -> snd x == snd y) xs) -- Aplica groupBy à lista xs, agrupando elementos consecutivos que têm a mesma segunda componente (palavra).
combineGroups xs = (map fst xs, snd (head xs)) --Para cada grupo de tuplas, cria uma nova tupla onde:
--O primeiro elemento é uma lista dos números das linhas (map fst xs).
--O segundo elemento é a palavra (snd (head xs)), que é a mesma para todas as tuplas no grupo.


shorten :: [([Int],Word')] -> [([Int],Word')] -- shorten recebe uma lista de tuplas contendo (lista de linhas de ocorrencia da palavra, palavra) e devolve um semelhante porém sem repetições na lista dentro da tupla
shorten xs = map removerduplas xs -- aplica map com a função removerduplas nos elementos de xs
removerduplas (x,y) = (nub x, y) -- aplica nub, que remove repetições de valores iguais, nos elementos x da tupla

makeindex :: Doc -> [([Int],Word')]
makeindex xs = shorten (almalgamate (sortLs (allNumWords (numLines (lines xs)))))