--4.1)

data Lista a = Nulo | a:>: (Lista a) deriving (Show,Eq)

removerElemento::(Eq a)=> a->  Lista a ->  Lista a
removerElemento _ Nulo =  Nulo
removerElemento c (x :>: xs) 
               |(x == c) = (removerElemento c xs ) 
               |(x/=c) = x:>:(removerElemento c xs) 

--4.2)

data Paridade = Par | Impar deriving Show

class ParImpar a where
    decide :: a -> Paridade

instance ParImpar Int where
    decide x
        | even x = Par
        | otherwise = Impar

instance ParImpar [a] where
    decide lista
        | (even . length) lista = Par 
        | otherwise = Impar 

instance ParImpar Bool where
    decide x
        | x = Par 
        | otherwise = Impar 


--4.3
-- Definindo o tipo TipoProduto
data TipoProduto = Escritorio | Informatica | Livro | Filme | Total deriving (Show, Eq)

-- Definindo o tipo Produto
data Produto = Produto { valor :: Double, tp :: TipoProduto } | Nada deriving (Show)

--4.4) 

data Arvore a = Nuloo | Folha a |Galho a (Arvore a) (Arvore a) deriving (Show)
emOrdem:: Arvore a -> [a]
emOrdem (Galho x l r) = emOrdem l ++ [x] ++ emOrdem r
emOrdem (Folha x) = [x]
emOrdem Nuloo = []

posOrdem:: Arvore a -> [a]
posOrdem (Galho x l r) = emOrdem l ++  emOrdem r ++ [x] 
posOrdem (Folha x) = [x]
posOrdem Nuloo = []

preOrdem:: Arvore a -> [a]
preOrdem (Galho x l r) = [x] ++ emOrdem l ++  emOrdem r
preOrdem (Folha x) = [x]
preOrdem Nuloo = []


somaarvore::(Num a)=> Arvore a -> a
somaarvore (Galho x l r) = ((somaarvore l) + x + (somaarvore r))
somaarvore(Folha x) = x
somaarvore Nuloo = 0
