-- MergeSort 
merge :: [Int] -> [Int] -> [Int]
merge l1 l2
	| l1 ==[] = l2
	| l2 ==[] = l1
	| (head l1) > (head l2) = (head l1):(merge (tail l1) l2 )
	| otherwise = (head l2):(merge l1 (tail l2) )
		
mySplit :: [Int] -> Int -> [[Int]]
mySplit l1 p
	| True =  [take p l1, (drop p  l1) ]
	
firstSplit :: [Int] -> Int -> [Int]
firstSplit l1 p
	| True = ( mySplit l1 p )!!0
	
secondSplit :: [Int] -> Int -> [Int]
secondSplit l1 p
	| True = ( mySplit l1 p )!!1	
	
mergeSort :: [Int] -> [Int]
mergeSort l1
	| l1==[] ||( (length l1)==1) = l1
	| otherwise = merge (mergeSort (firstSplit l1 (div (length l1) 2)  )) (mergeSort (secondSplit l1 (div (length l1) 2)  ))
	
	
-- HeapSort 
swap :: [Int] -> Int -> Int -> [Int] 
swap l1 i j
	| l1==[] || i==j  = l1 
	| j > i = (take (i) l1)++((l1!!j):[])++( drop (i+1) (take (j) l1 )  )++((l1!!i):[])++(drop (j+1) l1 )
	| otherwise = (take (j) l1)++((l1!!i):[])++( drop (j+1) (take (i) l1 )  )++((l1!!j):[])++(drop (i+1) l1 )

bubbleUp :: [Int] -> Int -> [Int]
bubbleUp l1 p
	| p==0 || l1==[] = l1 
	| (l1!!p) > (l1!!(div p 2) ) = bubbleUp (swap l1 p (div p 2) ) (div p 2)
	| otherwise = l1
	
insert :: [Int] -> Int -> [Int]
insert l1 v
	| l1==[] = v:[]
	| otherwise = bubbleUp (l1++(v:[])) ((length l1) )

heapFi :: [Int] -> Int -> [Int] 
heapFi l1 p
	|  2*p >= (length l1) = l1
	| ((2*p+1) < (length l1)) && (l1!!(2*p) < l1!!(2*p +1) )  && (l1!!(2*p +1) > (l1!!(p))) = heapFi (swap l1 p (2*p + 1) ) (2*p + 1)
	| (l1!!(2*p) > (l1!!(p)) ) = heapFi (swap l1 p (2*p) ) (2*p)
	| otherwise = l1
	
erase :: [Int] -> [Int]
erase l1 
	| l1 == [] = l1
	| otherwise = heapFi (take ((length l1)-1) (swap l1 0 ((length l1) - 1))) 0

getSorted :: [Int] -> [Int]
getSorted l1
	| l1==[] = [] 
	| otherwise = ((l1!!0):[])++(getSorted (erase l1) )

pvt_heapSort :: [Int] -> [Int] -> [Int]
pvt_heapSort l1 heap
	| l1 ==[] = getSorted heap
	| otherwise = pvt_heapSort (tail l1) (insert heap (head l1) )
	
heapSort :: [Int] -> [Int] 
heapSort l1
	| True = pvt_heapSort l1 []

	
	
	
	
-- SEGUNDA PARTE

menorMaior :: Int -> Int -> Int -> (Int,Int)
menorMaior a b c = (mergeSort([a,b,c])!!0 ,  mergeSort([a,b,c])!!2)


ordernaTripla :: (Int, Int, Int) ->  (Int, Int, Int)
ordernaTripla ( a, b, c ) = (mergeSort[a,b,c]!!0, mergeSort[a,b,c]!!1, mergeSort[a,b,c]!!2)

type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)
firstChord :: Ponto -> Float
firstChord (a,b) = a

secondChord :: Ponto -> Float
secondChord (a,b) = b

vertical :: Reta -> Bool
vertical (a,b) = firstChord(a)==firstChord(b)

--y = y1 + ( (x - x1)*(y2 - y1)/(x2 - x1) )
-- Quando a reta eh vertical, firstChord(b) == firstChord(a), entao podera ocorrer divisao por 0
pontoY :: Float -> Reta -> Float
pontoY x (a,b) = secondChord(a) + (  ((x-firstChord(a))*(secondChord(b) - secondChord(a) ))/(firstChord(b) - firstChord(a) ) )


doubleList xs = [2*a|a <- xs, (mod a 2) == 0]


type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa, Livro)]

getPessoa :: (Pessoa, Livro) -> Pessoa
getPessoa (a,b) = a
getLivro :: (Pessoa, Livro) -> Pessoa
getLivro (a,b) = b

baseExemplo :: BancoDados
baseExemplo = [("Sergio", "Livro 1"), ("Marlon", "Livro 2"), ("Fernando", "Livro 3"),  ("Tomer", "Livro 4"), ("Sergio", "Livro 5"), ("Carlos", "Livro 2")]


livros :: BancoDados -> Pessoa -> [Livro]
livros banco pessoa = [ getLivro(a)|a<- banco,  getPessoa(a) == pessoa]

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos banco livro = [ getPessoa(a)|a<- banco,  getLivro(a) == livro]


emprestado :: BancoDados -> Livro -> Bool
emprestado banco livro = length (emprestimos banco livro) > 0 

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos banco pessoa = length (livros banco pessoa)

emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar banco pessoa livro  
    | banco == []  = []
    | emprestado banco livro  = banco 
    | otherwise = banco++((pessoa, livro):[])

temLivro :: [Livro] -> Livro -> Bool
temLivro livros livro
    | livros == []  = False 
    | otherwise = ((head livros) == livro ) || temLivro (tail livros) livro


devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver banco pessoa livro  
    | banco ==[] = []
    | (temLivro (livros banco pessoa) livro) == False = banco
    | otherwise = [ a|a<- banco, ((getPessoa(a) == pessoa) == False) || ((getLivro(a) == livro) == False) ]


--QUICKSORT 
quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (h:t) =  quickSort([ a|a <- t, a < h  ])++(h:[])++quickSort([ a|a <- t, a >= h  ])


















	
	
	
	
	
	
	
	
	
	
	

