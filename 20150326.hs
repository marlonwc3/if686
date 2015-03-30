-- Parte 1 (Hash table ) 

type Value = String
type Hash = [Value]
type Key = String
type KeyCode = Int

-- Rabin-karp hash
basePrime :: Int 
basePrime = 1009 -- numero primo base

-- parse -- 

getVal :: Char -> Int  
getVal ch = fromEnum(ch) - fromEnum('a') + 1

auxEvaluate :: String -> Int -> Int -> Int -- caracteres minúsculos, O(m), onde m é o tamanho da string 
auxEvaluate "" sum p  = sum
auxEvaluate (h:t) sum p = auxEvaluate t (mod ((mod (27*sum) p) + getVal(h)) p ) p

-- Auxiliares do hash

getEvaluate :: Key -> KeyCode -- O(m), onde m é o tamanho da string 
getEvaluate str = auxEvaluate str 0 basePrime

expand :: Hash -> Int -> Hash -- O(len)
expand hash 0 = hash
expand hash len = expand (hash++("":[])) (len-1)

-- querys pós evaluate -- 
pvt_get :: Hash -> KeyCode -> Value -- O(basePrime)
pvt_get hash keyCode 
	| keyCode >= (length hash) = pvt_get (expand hash (keyCode - (length hash) + 300) ) keyCode 
	| otherwise = hash!!keyCode
	
pvt_put :: Hash -> (KeyCode, Value) -> Hash -- O(basePrime)
pvt_put hash (keyCode, value) 
	| keyCode >= (length hash) = pvt_put (expand hash (keyCode - (length hash) + 300) ) (keyCode,value)
	| otherwise =  (take keyCode hash)++(value:[])++(drop (keyCode+1) hash )

pvt_hasKey :: Hash -> KeyCode -> Bool -- O(basePrime)
pvt_hasKey hash keyCode = ( keyCode < length hash ) && length ( pvt_get hash keyCode ) > 0

pvt_remove :: Hash -> KeyCode -> Hash -- O(basePrime)
pvt_remove hash keyCode
	| keyCode >= (length hash)  = hash
	| otherwise = pvt_put hash (keyCode, "")

-- Querys do trabalho 
get :: Hash -> Key -> Value -- O(basePrime)
get hash key = pvt_get hash (getEvaluate key)
	
put :: Hash -> (Key, Value) -> Hash -- O(basePrime)
put hash (key,value) = pvt_put hash (getEvaluate key, value)

hasKey :: Hash -> Key -> Bool -- O(basePrime)
hasKey hash key = pvt_hasKey hash (getEvaluate key) 

remove :: Hash -> Key -> Hash -- O(basePrime)
remove hash key = pvt_remove hash (getEvaluate key) 



-- Parte 2 ( Igualdade de conjuntos ) 

exist :: (Eq t) => [t] -> t -> Bool -- O(n) 
exist set element = length ( [ a|a <- set , a == element  ] ) > 0

--equal :: Set -> Set -> Bool
removeEquals :: (Eq t) => [t] -> [t] -> [t] -- O(n²)
removeEquals [] res = res
removeEquals (h:t) res 
	| exist res h = removeEquals t res
	| otherwise = removeEquals t (res++(h:[]))


eqSet :: (Eq t) => [t] -> [t] -> String -- O(n²)
eqSet setA setB 
	| (((length setE) == (length setC)) && ((length setE) > (length setD))) = "A contem B"
	| (((length setE) == (length setD)) && ((length setE) < (length setC))) = "A contem B"			
	| (((length setE) == (length setC)) && ((length setE) < (length setD))) = "B contem A"		
	| (((length setE) == (length setD)) && ((length setE) > (length setC))) = "B contem A"
	| ( (length setC > 0) && (length setD > 0 ) ) && length setE == 0 = "Conjuntos Disjuntos"
	| (((length setE) == (length setC)) && ((length setE) == (length setD))) = "A igual a B!"
--	| (((length setE) < (length setD)) && ((length setE) < (length setC))) = "A interseciona B"
	| otherwise = "A interseciona B"
	where setC = removeEquals setA []; setD = removeEquals setB []; setE = [ a|a <- setC , exist setD a ]
	
	
	
-- PARTE DA AULA --



myTake :: Int -> [a] -> [a]
myTake 0 lista = lista 
myTake n lista
	| n >= (length lista) = lista
	| otherwise = (h:[])++(myTake (n-1) t) 
	where h = head lista; t = tail lista;

myDrop :: Int -> [a] -> [a]
myDrop 0 lista = lista
myDrop n lista 
	| n >= (length lista) = lista
	| otherwise = (myDrop (n-1) t) 
	where h = head lista; t = tail lista;

myTakeWhile :: [a] -> (a -> Bool) -> [a]		
myTakeWhile [] f = []
myTakeWhile lista f 
	| f(h) == False = [] 
	| otherwise = (h:[])++( myTakeWhile t f)
	where h = head lista; t = tail lista;

myDropWhile :: [a] -> (a -> Bool) -> [a]		
myDropWhile [] f = []
myDropWhile lista f 
	| f(h) == False = t
	| otherwise = ( myDropWhile t f)
	where h = head lista; t = tail lista;


quickSort :: (Ord x) => [x] -> [x]
quickSort [] = []
quickSort (h:t) =  quickSort([ a|a <- t, a < h  ])++(h:[])++quickSort([ a|a <- t, a >= h  ])


qnt :: (Eq t) => [t] -> t -> Int
qnt [] x = 0
qnt lista x 
	| h==x = 1 + (qnt t x)
	| otherwise = (qnt t x)
	where h = head lista; t = tail lista;
	
--Ultima questao do slide

insert :: Eq t => [(t, Int)] -> t -> [(t, Int)]
insert [] v = [(v,1)]
insert (h:t) v 
	| ((fst h) == v) = ([( fst h, (snd h) + 1)])++t
	| otherwise = [h]++(insert t v)

insertAll :: Eq t => [t] -> [(t, Int)] -> [(t, Int)]
insertAll [] res = res
insertAll (h:t) res = insertAll t (insert res h)

listTest :: [String]
listTest = ["abba", "ccc", "abc", "def", "z", ""]

pvt_agrupar :: Eq t => [[t]] -> [(t, Int)] -> [(t, Int)]
pvt_agrupar [] res = res
pvt_agrupar (h:t) res = pvt_agrupar t (insertAll h res)

agrupar :: Eq t => [[t]] -> [(t,Int)]
agrupar lista = pvt_agrupar lista []

	

















	
	
	
	
	
	

