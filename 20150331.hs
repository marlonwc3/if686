{- 
1ª) 
O polimorfismo de haskell é mais seguro quanto a garantia de
que os tipos terão implementações das funcionalidades que serão 
utilizadas sob eles, pois é possível restringir que os argumentos genéricos 
possuam implementações de determinadas funções, sendo esses definidos por uma
classe. Em Java, o Generics que é utilizado no polimorfismo acaba sendo mais 
geral pois não restringe os dados, porém é mais sucetível à erro tendo em vista que
apenas ocorre uma verificação de tipo em tempo de execução e depois ocorre um "type erasure"
para gerar compatibilidade com as JVM antigas, pois Generics foi adicionada a partir do Java 5.
-}

-- 2ª
getVal :: Char -> Int  
getVal ch = fromEnum(ch) - fromEnum('0')

parseInteger :: String -> Int -> Int
parseInteger [] sum = sum
parseInteger (h:t) sum = parseInteger t (10*sum + getVal(h) )

pvt_getNext :: String -> Char -> Int -> String
pvt_getNext [] last qt = (show qt)++([last])
pvt_getNext (h:t) last qt
	|  (h == last)  = pvt_getNext t last (qt+1)
	| otherwise = (show qt)++([last])++( pvt_getNext t h 1  )
	
getNext :: String -> String 
getNext "" = ""
getNext (h:t) = pvt_getNext t h 1


pvt_fooB :: Int -> String -> Int
pvt_fooB 0 curr = parseInteger curr 0
pvt_fooB n curr = pvt_fooB (n-1) (getNext curr)

foo :: Enum n => n -> Int -- Função que calcula o n-esimo numero, da estouro para n>=10 , uma saída é retornar uma String, como BigInteger de Java
foo n = pvt_fooB (fromEnum(n)-1) "1"


-- 3ª (Busca no grafo)
{-
A busca recebe um grafo (não necessariamente conexo e direcionado), que é formado por uma tupla: (lista de adjacência, lista de valores) 
a lista define a estrutura do grafo e os valores mapeiam à cada nó um rótulo, logo cada nó
está mapeado em um número ( índice na lista de valores ) 

A busca funciona da seguinte forma:
Recebe o grafo, 2 rótulos,
mapeia cada rótulo em seu respectivo ID (inteiro)
Executa uma busca com força bruta O(2^n) 
Encontra qualquer caminho qualquer que chegue no destino, 
esse caminho possui os nós em seus mapeamentos (rótulo --> inteiro )
Então a resposta é o caminho após um processamento para re-mapear os índices nos seus rótulos.

adjList e graphValues devem possuir o mesmo length obviamente, pois
adjList[i] == lista de adjacência do i-ésimo nó
graphValues[i] == valor do i-ésimo nó

Nesse caso a lista de adjacência é uma lista de lista de inteiros, 
uma maneira de deixar mais genérico seria preprocessar uma lista de lista de rótulos em uma lista de lista de inteiros.

Usando: search grafo src dst
-}
type Rotule n = n -- Tipo do rótulo 
type GraphValues n = [Rotule n] 
type AdjList = [[Int]]
type Graph n = (AdjList, GraphValues n)

-- 1-indexed
pvt_get :: Eq n => [n] -> n -> Int
pvt_get [] r = -10000000
pvt_get (h:t) r
	| h == r = 1
	| otherwise = 1 + (pvt_get t r)

setMark :: [Int] -> Int -> Int -> [Int]
setMark [] id v = []
setMark	 mark id v = (take (id-1) mark)++([v])++(drop id mark)

genMark :: Int -> [Int]
genMark 0 = []
genMark n = ([0])++(genMark (n-1) )

pvt_search :: AdjList -> [Int] -> Int -> Int -> [Int]
pvt_search [] mark src dst = []
pvt_search adjList mark src dst
	| src == dst = [dst]
	| (length myAdjList == 0) = []
	| (length allPaths) == 0 = []
	| otherwise = ([src])++(allPaths!!0)
	where myAdjList = adjList!!(src-1); myMark = (setMark mark src 1) ; possibles = [ pvt_search adjList myMark a dst |a<-myAdjList, (myMark!!(a-1))==0   ]; allPaths = [ a|a <- possibles, length a > 0]

mapPath :: Eq n => [n] -> [Int] -> [n]
mapPath [] path = []
mapPath graphValues [] = []
mapPath graphValues (h:t) =  ([graphValues!!(h-1)])++(mapPath graphValues t)

search :: Eq n => ([[Int]], [n]) -> n -> n -> [n] -- função de uso
search graph src dst
	| srcMap < 0 || dstMap < 0 = []
	| (fst graph == []) = []
	| otherwise = mapPath graphValues path
	where adjList = (fst graph); graphValues = (snd graph); mark = genMark ( length adjList  ); srcMap = pvt_get graphValues src; dstMap = pvt_get graphValues dst; path = pvt_search adjList mark srcMap dstMap;

-- Sample de teste
baseAdjList :: AdjList
baseAdjList = [ [4,2], [1,3], [2, 5 , 6], [1], [3, 6], [3, 5], [8], [7] ] 

baseGraphValues :: Eq n => [n] -> [n]
baseGraphValues a = a

baseGraph ::Eq n => ([[Int]], [n]) -> ([[Int]], [n])
baseGraph a = a	

baseMark :: [Int]
baseMark = genMark (( length baseAdjList )+3) 
	

-- 4ª Questao 
-- uso: filtroMediana matrix n 

-- ordenar a lista da mediana
quickSort :: (Ord x) => [x] -> [x]
quickSort [] = []
quickSort (h:t) =  quickSort([ a|a <- t, a < h  ])++(h:[])++quickSort([ a|a <- t, a >= h  ])

mediana :: [Int] -> Int
mediana [] = 0
mediana mylist
	| (mod (length lista) 2) == 1 = lista!!( (div (length lista) 2)  ) 
	| otherwise = div ((lista!!(div (length lista) 2 ))+(lista!!( (div (length lista) 2)-1  ) )) 2
	where lista = quickSort mylist

iterOver :: [x] -> Int -> Int -> [x]
iterOver [] a b = []
iterOver lista a b
	| a > b || b<= 0 ||  ( a > (length lista )) = []
	| a <= 0 = (iterOver lista (a+1) b)	
	| otherwise = ([lista!!(a-1)])++(iterOver lista (a+1) b)

getAll :: [[Int]] ->  [Int]
getAll [] = []
getAll (h:t) = h++(getAll t)

getMediana :: [[Int]] -> Int -> Int -> Int -> Int
getMediana matrix i j n = mediana all
	where rows = (iterOver matrix (i-(n-1)) (i+(n-1))); cols = [ (iterOver a (j-(n-1)) (j+(n-1))) |a <- rows  ]; all = getAll cols


pvt_filtroMediana :: [[Int]] -> Int -> Int -> Int -> [Int] -> [[Int]]
pvt_filtroMediana [] i j n col = []
pvt_filtroMediana matrix i j n col
	| j > (length (matrix!!0)) = ([col])++(pvt_filtroMediana matrix (i+1) 1 n [] ) 
	| i > (length matrix ) = []
	| otherwise = pvt_filtroMediana matrix i (j+1) n colAux
	where med = (getMediana matrix i j n); colAux = col++[med]

filtroMediana :: [[Int]] -> Int -> [[Int]]
filtroMediana matrix n = pvt_filtroMediana matrix 1 1 n []

baseMatrix :: [[Int]] 
baseMatrix = [[1,2,3],[4,5,6], [7,8,9]]

