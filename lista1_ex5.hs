data Nat = Z | Succ Nat | Ant Nat deriving Show

convNat :: Nat -> Int
convNat Z = 0
convNat (Succ n) = 1 + convNat n
convNat (Ant n) = convNat n - 1

soma :: Nat -> Nat -> Nat 
soma Z x = x
soma (Succ n) x = Succ(soma n x)
soma (Ant n) x = Ant(soma n x)

convInt :: Int -> Nat
convInt 0 = Z
convInt x
    | x > 0 = Succ (convInt (x-1))
    | x < 0 = Ant (convInt (x+1))

mult :: Nat -> Nat -> Nat
mult (Z) (x) = Z 
mult (Succ n) x = soma (mult n x) x
mult (Ant n) x = soma (mult n x) x

fib :: Nat -> Nat
fib Z = Succ Z
fib (Succ Z) = Succ Z
fib (Succ(Succ x)) = soma (fib(Succ x)) (fib x)
