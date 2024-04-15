{-
 
ej 1, implementar la funcion fibonacci 

-}

fibonacci :: Int -> Int
fibonacci i
    | i == 0 = 0
    | i == 1 = 1
    | i < 0 = error "tiene que ser positivo"
    |otherwise = fibonacci (i-1) + fibonacci (i-2)

{-

ej 2 devolver parte entera de un numero con coma.

-}

parteEntera :: Float -> Int
parteEntera x = round x


{-

Especificar e implementar la funci´on esDivisible :: Integer ->Integer ->Bool que dados dos n´umeros
naturales determinar si el primero es divisible por el segundo. No est´a permitido utilizar las funciones mod ni div


a/b = c

-}

modulo :: Int -> Int -> Int
modulo a b 
    |a >= 0 && b > a = a
    |otherwise = modulo (a-b) b 

esDivisible :: Int -> Int -> Bool
esDivisible a b = modulo a b == 0

-- preguntat si este esta bien, como hacerlo. Si puedo yo crear mi mod y mi div o si es trampa

{-

Ejercicio 4. Especificar e implementar la funci´on sumaImpares :: Integer ->Integer que dado n ∈ N sume los primeros
n n´umeros impares. Por ejemplo: sumaImpares 3 ❀ 1+3+5 ⇝ 9.

-}

sumaImpares:: Int -> Int
sumaImpares x 
    | x <= 0 = 0
    | x `mod` 2 == 0 = sumaImpares (x-1)
    | otherwise = sumaImpares (x-2) + x

{-



-}