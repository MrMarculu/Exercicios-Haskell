data Dinheiro a =   Dolar a |
                    Real  a 

-- Como eu quero que só seja representado de maneira númerica:
instance (Num a, Show a) => Show (Dinheiro a) where
    show (Dolar x) = "Dolar " ++ show x
    show (Real  x) = "Real  "  ++  show  x

convDolar :: (Fractional a) => [Dinheiro a] -> [Dinheiro a]
convDolar = map funcaoConverteDolar
    where 
        taxaCambio = 0.2
        funcaoConverteDolar (Dolar val) = Dolar val
        funcaoConverteDolar (Real val) = Dolar (val * taxaCambio)

convReal :: (Fractional a) => [Dinheiro a] -> [Dinheiro a]
convReal = map funcaoConverteReal
    where
        taxaCambio = 4.95
        funcaoConverteReal (Real val) = Real val
        funcaoConverteReal (Dolar val) = Real (val * taxaCambio)
 
--Letra a
filDolar :: (Fractional a) => [Dinheiro a] -> [Dinheiro a]
filDolar = filter eDolar
    where
        eDolar (Dolar _) = True
        eDolar _ = False

--Letra b
somaDinheiro :: (Fractional a) => [Dinheiro a] -> Dinheiro a
somaDinheiro = foldr funcaoSoma (Dolar 0). filDolar
    where
        funcaoSoma (Dolar val) (Dolar ant) = Dolar (val + ant)

-- Letra c
aumentarReais :: (Fractional a) => [Dinheiro a] -> a -> [Dinheiro a]
aumentarReais listaDinheiro aumento = map adiciona listaDinheiro
    where
        adiciona (Real valor)  = Real (valor + aumento)
        adiciona (Dolar valor) = Dolar valor