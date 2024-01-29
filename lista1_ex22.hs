data Cripto = Mensagem [Char] | Cifrado [Char] | Error String
                deriving Show

encriptar :: Cripto -> Cripto
encriptar (Mensagem msg) = Cifrado (map succ msg)
encriptar _  = Error "nao e possivel encriptar"

decriptar :: Cripto -> Cripto
decriptar (Cifrado msg) = Mensagem (map pred msg)
descriptar _ = Error "nao e possivel descriptar"