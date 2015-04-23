
-- TRABALHO 8 -- 

quickSort :: Ord t => [t] -> [t] 
quickSort [] = []
quickSort l = (quickSort([a | a <- l, a < pivo]))++[pivo]++(quickSort([a | a <- t, a >= pivo]))
	where pivo = l!!0; t = tail l; 


getLE ::(Ord a, Num a) =>  a -> [a] -> Int -> [a] -> ([a], Int)
getLE v [] p r = (r, p)
getLE v l p r
	| p > (length l) = (r, p)
	| (l!!(p-1)) <= v = getLE v l (p+1) (r++[l!!(p-1)])
	| otherwise = (r, p)
	

go :: (Ord a, Num a) => [a] -> [a] -> Int -> Int ->  [[a]]
go [] b i j = []
go a b i j
	| j > length b = []
	| i > length a = [ drop (j-1) b ]
	| (length l) == 0 = go a b (i+1) j
	| otherwise = ([l])++(go a b (i+1) next_j)
	where iesimo = a!!(i-1); res = getLE iesimo b j []; next_j = snd res; l = fst res;


listPartitioner :: (Ord a, Num a) => [a] -> ( [a] -> [[a]]  )
listPartitioner a = f
	where f b = go (quickSort a) (quickSort b) 1 1

{-
let f = listPartitioner [4, 13, 8]
f [1..15]
-}

-- EXERCICIOS DA AULA -- 

mapFilter :: (a -> b ) -> (a -> Bool ) -> [a] -> [b]
mapFilter f g l = [f a | a <- l , g a ]

-- \m -> 3 + m

addNum n = (\m -> n+m)
firstOnes lista = (\n -> take n lista )

maioresQue listas = (\n -> [a | a <- listas , (length a) >n  ]  )


myexist [] = (\x -> False ) 
myexist (h:t) = (\x -> (h==x ) || ( myexist t x) )

juntar [] = (\x -> x) 
juntar (h:t)  = (\x -> juntar t (x++h))  

myuni (h:t)  = (\x -> myexist x h)

remDup :: Eq a => [a] -> [a] -> [a] 
remDup [] ans = ans
remDup (h:t) ans
	| myexist ans h = remDup t ans 
	| otherwise = remDup t (ans++[h])
	
	
myUni :: Eq a =>  [[a]] -> [a] 
myUni l = remDup (juntar l [] ) [] 
 
 













