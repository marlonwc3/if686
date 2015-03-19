double :: [Int] -> [Int]
double as
	| as == [] = []
	| otherwise = (head as)*2:double(tail as)

member :: [Int] -> Int -> Bool
member as val
	| as == [] = False
	| otherwise = ( (head as) == val ) || member (tail as) val
	
digits :: String -> String
digits as 
	| as == [] = ""
	| (head as >= '0' && head as <= '9') = (head as):digits(tail as)	
	| otherwise = digits(tail as)
	
sumPairs :: [Int] -> [Int] -> [Int] 
sumPairs l1 l2 
	| l1 == [] = l2
	| l2 == [] = l1
	| otherwise = ((head l1) + (head l2)):(sumPairs (tail l1) (tail l2) )
	
	
pegueMaiores :: [Int] -> Int -> [Int]
pegueMaiores l1 val
	| l1 == [] = []
	| ((head l1) > val) = (head l1):( pegueMaiores (tail l1) val )
	| otherwise = pegueMaiores (tail l1) val
	
pegueMenores :: [Int] -> Int -> [Int]
pegueMenores l1 val
	| l1 == [] = []
	| ((head l1) <= val) = (head l1):( pegueMenores (tail l1) val )
	| otherwise = pegueMenores (tail l1) val	

quickSort :: [Int] -> [Int]
quickSort l1
	| l1 == [] = []
	| otherwise = (quickSort(pegueMenores (tail l1) (head l1)))++((head l1):[])++ quickSort( (pegueMaiores (tail l1) (head l1)) ) 
	
	
merge :: [Int] -> [Int] -> [Int]
merge l1 l2
	| (l1 == []) = l2
	| (l2 == []) = l1 
	| ( (head l1) > (head l2) ) = ( head(l1):[] )++( merge (tail l1) l2 )
	| otherwise = ( head(l2):[] )++( merge l1 (tail l2) )



mergeSort :: [Int] -> Int -> Int -> [Int]
mergeSort l1 lo hi
	| (l1 == []) = l1	
	| ((lo+1)>=hi) = []
	
	
	
	





	


	
