data Mes =   Jan 
            |Fev 
            |Mar 
            |Abr 
            |Mai 
            |Jun 
            |Jul 
            |Ago 
            |Set 
            |Out 
            |Nov 
            |Dez 
            deriving (Enum,Eq,Show)

data Estacao = Ver | Outo | Inv | Prim
data Hemisferio = Norte | Sul 

-- olhar apenas os meses: fev e com 31 dias, caso nao seja nenhum
-- ele tem 30 dias
checaFim :: Mes -> Int
checaFim mesDesejado =
    case mesDesejado of
        Jan -> 31
        Fev -> 28
        Mar -> 31
        Mai -> 31
        Jul -> 31
        Ago -> 31
        Out -> 31
        Dez -> 31
        _   -> 30

prox :: Mes -> Mes
prox Dez = Jan
prox mesDesejado = succ mesDesejado

estacao :: Mes -> Hemisferio -> Estacao
estacao mesDesejado hemisferio = case hemisferio of
    Norte -> case hemisferio of 
            _  | mesDesejado `elem` mesInv -> Inv
               | mesDesejado `elem` mesPrimavera -> Prim
               | mesDesejado `elem` mesVerao -> Ver
               | mesDesejado `elem` mesOutono -> Outo
    -- Troca de ordem para Norte e Sul 
    Sul -> case mesDesejado of
            _  | mesDesejado `elem` mesVerao -> Inv
               | mesDesejado `elem` mesOutono -> Prim
               | mesDesejado `elem` mesInv -> Ver
               | mesDesejado `elem` mesPrimavera -> Outo 
    where
    -- meses com base no norte
    mesInv = [Jan,Fev,Mar]
    mesPrimavera = [Abr,Mai,Jun]
    mesVerao = [Jul,Ago,Set]
    mesOutono = [Out,Nov,Dez]