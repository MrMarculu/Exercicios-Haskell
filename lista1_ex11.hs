
-- Questão 1.1
-- (a) Gerar uma lista com os números 0, 5, 10, ..., 50
listaA :: [Int]
listaA = [0,5..50]

-- (b) Criar uma lista de 'a' a 'z', excluindo as vogais
listaB :: [Char]
listaB = [c | c <- ['a'..'z'], not (c `elem` "aeiou")]

-- (c) Uma lista de 0 a 50 sem os números 2, 7, 13, 35 e 42
listaC :: [Int]
listaC = [x | x <- [0..50], not (x `elem` [2, 7, 13, 35, 42])]

-- (d) Uma lista com todas as coordenadas de um tabuleiro de damas 8x8
listaD :: [(Char, Int)]
listaD = [(letra, numero) | letra <- ['a'..'h'], numero <- [1..8]]

-- (e) Função para verificar se o tamanho de uma String é par
ehPar :: String -> Bool
ehPar str = even (length str)

