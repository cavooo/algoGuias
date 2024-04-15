todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (a,b) (c,d) = a > c && b > d




{-el _ se usa como comodin, 

Float es real, integer es enteros!!

patter mathching matchea (a,b) (c,d) con (Float,Float) (Float, Float)
todoMenor (a,b) (c,d) = a > c && b > d = True

posPrimerPar: dada una terna de enteros, devuelve la posici´on del primer n´umero par si es que hay alguno, y devuelve
4 si son todos impares

-}

posPrimerPar :: (Int, Int) -> (Int, Int) -> Int
posPrimerPar (a,b) (c,d)
    |a `mod` 2 == 0 = 1
    |b `mod` 2 == 0 = 2
    |c `mod` 2 == 0 = 3
    |d `mod` 2 == 0 = 4
    |otherwise = 4

posPrimerPar2 :: (Int, Int) -> (Int, Int) -> Int
posPrimerPar2 q p
    |fst q `mod` 2 == 0 = 1
    |snd q `mod` 2 == 0 = 2
    |fst p `mod` 2 == 0 = 3
    |snd p `mod` 2 == 0 = 4
    |otherwise = 4


-- ej 7 

{-

Ejercicio 7. Implementar una funci´on:
distanciaManhattan:: (Float, Float, Float) ->(Float, Float, Float) ->Float
problema distanciaManhattan (p : R × R × R, q : R × R × R) : R {
requiere: {T rue}
asegura: {res = P2
i=0 |pi − qi|}
}
Por ejemplo:
distanciaManhattan (2, 3, 4) (7, 3, 8) ⇝ 9
distanciaManhattan ((-1), 0, (-8.5)) (3.3, 4, (-4)) ⇝ 12.8

-}

esNegativo :: Float -> Float
esNegativo x
    |x < 0 = -x
    |otherwise = x

distanciaManhattan :: (Float, Float, Float) -> (Float, Float, Float) -> Float
distanciaManhattan (a,b,c) (d,e,f) = esNegativo(a-d) + esNegativo(b-e) + esNegativo(c-f)

--d)

f4 :: Float -> Float -> Float
f4 x y = ( x + y )/2

--e) 

f5 :: ( Float , Float ) -> Float
f5 (x , y ) = ( x + y )/2

