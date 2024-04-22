-- (eq t) -> t -> [t] -> Bool

pertenece ::(Eq t) => t -> [t] -> Bool -- declaras que t es de Eq, que significa que sabe compararse
pertenece _ [] = False
pertenece e (x:xs) = e == x || pertenece e xs


{-
    si la lista es de 2 y vos le das (x:y:z:xs), va a dar mal
    x seria el primero, y el segundo, xs seria el tercero pero solo en pattern matching que sabes que s una lista de 3
-}


longitud :: [t] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

ultimo :: (Eq t) => [t] -> t
ultimo [x] = x
ultimo (_:xs) = ultimo xs

principio :: (Eq t) => [t] -> [t]
principio [x] = []
principio (x:xs) = (x : principio xs)

-- subseq (t, 0 , |t-1|) subseq toma una lista (t), devuelve el t[0] inclusive, y el |t-1| exclusive
-- entonces de una lista t devuelve del index 0 al ultimo sin incluir

-- acordate de usar :t (:) ej 1 : [1,2,3] = [1,1,2,3] lo agrega al inicio pero es t -> [t] -> [t]
-- y :t (++) ej [1,2,3,4] ++ [4] = [1,2,3,4,4] lo agrega al final pero es [t] -> [t] -> [t]


reverso :: [t] -> [t]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x] 


-- [1,1,1,1]
todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True
todosIguales [_] = True
todosIguales (x:xs) = x == ultimo xs && todosIguales (principio xs)

todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [] = True 
todosDistintos [_] = True 
todosDistintos (x:xs) = not (x == primero xs && todosDistintos xs)
    where primero (x:xs) = x

-- [1,1,1,1] == a su reverso
-- [1,2,2,1] == a su reverso

hayRepetidos :: (Eq t) => [t] -> Bool 
hayRepetidos t = not (todosDistintos t)


quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar n (x:xs) 
    |n == x = xs
    |otherwise = x : quitar n xs

quitarTodos :: (Eq t) => t -> [t] -> [t]
quitartodos _ [] = []
quitarTodos n (x:xs)
    |n == x = quitarTodos n (xs)
    |otherwise = x : quitar n xs

{-

3 [1,2,3]

-}