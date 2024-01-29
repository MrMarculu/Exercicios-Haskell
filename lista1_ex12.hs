-- Questão A
textoEpar :: String -> Bool
textoEpar = even . length

-- Questão B
reversoListString :: [String] -> [String]
reversoListString = map reverse

-- Questão C
headComposicao :: [a] -> a
headComposicao =  last . take 1

-- Questão D
intBinario :: Int -> [Int]
intBinario 0 = [0]
intBinario 1 = [1]
intBinario x = intBinario(div x 2) ++ [mod x 2]