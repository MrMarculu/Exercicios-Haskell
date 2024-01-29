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

{-
• Implemente os percursos pos-ordem e pre-ordem. Via comentario, faca os ”testes de mesa” para os dois
percursos da arvore Galho 15 (Galho 11 (Folha 6) (Galho 12 (Folha 10) Nuloo)) (Galho 20 Nuloo (Galho 22 (Folha 21) Nuloo))
• Usando a estrutura de arvore vista, faca uma funcao que some todos os elementos de uma arvore de numeros.

Teste de mesa em posOrdem 
1) Raiz 15 (l) (r) => [6]++ [10] ++ [] ++ [12] ++ [11] ++ [] ++  [21] ++ [] ++ [22] ++ [20]
        l.1)(l)-> Raiz 11 (l) (r) => [6]++ [10] ++ [] ++ [12] ++ [11]
                l.2) (l)-> Folha 6  =>Retorna [6]
                l.2) (r)-> Raiz 12 (l) (r) => Retorna [10] ++ [] ++ [12]
                    l.3) (l)-> Folha 10 => Retorna [10]
                         (r)-> Nulo  => Retorna []  
        r.1)(r)-> Raiz 20 (l) (r) => [] ++  [21] ++ [] ++ [22] ++ [20]
                r.1) (l)-> Nulo => Retorna []
                     (r)-> Raiz 22 l r => Retorna [21] ++ [] ++ [22] 
                        r.2) (l)-> Folha 21 => Retorna [21]
                             (r)-> Nulo => Retorna []

Teste de mesa em preOrdem  [x] l r
2) Raiz 15 (l) (r) => [15]++ [11] ++ [6] ++ [12] ++ [10] ++ [] ++ [20] ++ [] ++ [22] ++ [21] ++ []
        l.1)(l)-> Raiz 11 (l) (r) => [11] ++ [6] ++ [12] ++ [10] ++ []
                l.2) (l)-> Folha 6  => Retorna [6]
                l.2) (r)-> Raiz 12 (l) (r) => [12] ++ [10] ++ []
                    l.3) (l)-> Folha 10 => Retorna [10]
                         (r)-> Nulo  => Retorna []  
        r.1)(r)-> Raiz 20 (l) (r) => [20] ++ [] ++ [22] ++ [21] ++ []
                r.1) (l)-> Nulo =>Retorna [] 
                     (r)-> Raiz 22 l r => Retorna [22] ++ [21] ++ []
                        r.2) (l)-> Folha 21 => Retorna [21]
                             (r)-> Nulo => Retorna []
                 

 -}

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
