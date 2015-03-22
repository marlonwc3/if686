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

	
	
	
	
	
	
	
	
	
	
	
	

