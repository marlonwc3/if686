qtdVendas :: Int -> Int
qtdVendas n = 2*n 

toInt True = 1
toInt False = 0

f :: Int -> Int -> Int 
f s n
	| n < 0 = 0
	| otherwise = toInt (  s == qtdVendas(n)  ) + (f s (n-1) )
	

{-
-> 2 e 1 respectivamente
-> lista de listas
-> [2,4,5,6,7,8,9]
-> [2]
-> [2]
-> [10]
-> []
-> [2,9]
-}
	
	
