myfoldr :: (t -> u -> u) -> u  -> [t] -> u
myfoldr f s [] = s
myfoldr f s (h:t) = f h (myfoldr f s t)


member :: Eq t => t -> [t] -> Bool
member x lista = myfoldr (||) False (map (==x) lista )

myremove :: Eq t => [t] -> [t] -> [t]
myremove [] b = b
myremove a [] = []
myremove a (h:t) 
	| not (member h a) = [h]++(myremove a t) 
	| otherwise = myremove a t
	
myunion :: Eq t => [t] -> [t] -> [t]
myunion a b = foldr (++) [] ([a,(myremove a b)])	

toInt :: Char -> Int
toInt a
	| val >= 97 = val - 97 + 1
	| otherwise = val - 65 + 1
	where val = fromEnum a;
	
mymap :: String -> Int
mymap [] = 0
mymap (h:t) = (toInt h) + (mymap t)

toIntegers :: [String] -> [Int]
toIntegers l = foldr (:) [] (map mymap l)

data Tree t = Nil | Tree [t] deriving (Ord, Eq, Show)

pvt_insertTree :: Ord t => Tree t-> t -> Int -> Tree t
pvt_insertTree Nil x p = (Tree [x])
pvt_insertTree (Tree tree) x p
	| p > (length tree) = (Tree (tree++[x]))
	| x < ( tree!!(p-1) ) = pvt_insertTree (Tree tree) x (2*p)
	| otherwise = pvt_insertTree (Tree tree) x ((2*p) + 1) 




-- pvt_inserTree [] 5 1
insertTree :: Ord t => t -> Tree t -> Tree t
insertTree x Nil = (Tree [x])
insertTree  x (Tree tree) = pvt_insertTree (Tree tree) x 1


criarArvore :: Ord t => [t] -> ( t -> Tree t -> Tree t) -> Tree t
criarArvore l f = foldr f Nil l

--criarArvore [1,2,3] insertTree
{-
myfoldr :: (t -> u -> u) -> u  -> [t] -> u
myfoldr f s [] = s
myfoldr f s (h:t) = f h (myfoldr f s t)
-}

myIsDigit :: Char -> Bool
myIsDigit c = fromEnum(c) >= 48 && fromEnum(c) <= 57

digits l = filter myIsDigit l


mysum :: [Int] -> Int
mysum [] = 0
mysum (h:t) = h + (mysum t)


myRemove :: [[Int]] -> Int -> [[Int]]
myRemove l v = foldr (:) [] (filter filtro l)
	where filtro k = ((mysum k) >= v) ; 


removeDuplicado :: Eq t => [t] -> [t] -> [t] 
removeDuplicado [] l = l
removeDuplicado (h:t) l
	| member h l = removeDuplicado t l
	| otherwise = removeDuplicado t (l++[h])


inter :: Eq t => [t] -> [t] -> [t]
inter a b = removeDuplicado res []
	where filtro k = length ([ x | x <- (a++b), (member x a ) && (member x b) && x==k ]) > 0;
		  res = filter filtro (a++b); 

diff :: Eq t => [t] -> [t] -> [t]
diff a b = removeDuplicado res []
	where rinter = inter a b; filtro n = not (member n rinter); res = filter filtro (a++b)


-- TRABALHO 7 -- 

compose :: (u->v) -> [(t->u)] ->[(t->v) ]
compose g l = [ (g .a )|a<- l ] 

-- (head ( tail ( compose ( + 1 ) [(* 2), (/ 2), (+ 4)]))) 5

data Graph t = Empty | Graph [[(Int, Int)]] [t] deriving (Show, Eq)

baseGraph :: Graph String 
--baseGraph = Graph [[(2,3)], []] ["A", "B"]
baseGraph = Graph [[(2,5),(3,1), (6,5)], [(4,2)], [(4,3),(5,1)], [(1,2)], [(3,4)], [] , []] ["A", "B", "C", "D", "E", "F", "G"]

mapGraph :: Graph t -> (t->v) -> Graph v
mapGraph Empty f = Empty
mapGraph (Graph adjList values) f = (Graph adjList ([f a | a <- values]))


foo :: String -> Int
foo s = (toIntegers [s])!!0
-- mapGraph baseGraph foo


foldGraph :: (Graph t-> Graph u-> Graph u) -> Graph u -> Graph t -> Graph u
foldGraph f b Empty = b
foldGraph f b (Graph adjList values) = f (Graph [hAdj] [hValues]) (foldGraph f b (Graph tAdj tValues) )
	where hAdj = head adjList; tAdj = tail adjList; hValues = head values; tValues = tail values;


erase :: [Int] -> Int -> [Int] 
erase l p = (take (p-1) l)++(drop p l)


go :: Ord t => [Bool] -> Tree t -> Int -> [Int]
go l Nil p = []
go [] tree p = []
go todos (Tree tree) p
	| p > (length tree ) = []
	| not (todos!!(p-1)) = []
	| otherwise = [p]++(go todos (Tree tree) (2*p))++(go todos (Tree tree) ((2*p) +1))
	
getFirst :: [Bool] -> Int -> Int
getFirst [] p = p
getFirst (h:t) p
	| h = p
	| otherwise = getFirst t (p+1)
	
toOn :: [Bool] -> Int -> [Bool]
toOn [] p = []
toOn l p = (take (p-1) l)++[True]++(drop p l)
	
ative :: [Bool] -> [Int] -> [Bool]
ative [] pos = []
ative at [] = at
ative at (h:t) = ative (toOn at h) t
	
get :: [t] -> [Int] -> [t] 
get [] pos = []
get l [] = []
get l (h:t) = [(l!!(h-1))]++(get l t)

getAll :: Ord t => Tree t -> [Bool] -> [Tree t]
getAll Nil l = []
getAll tree [] = []
getAll (Tree tree) l
	| first > (length l) = []
	| otherwise = [(Tree (get tree nova))]++(getAll (Tree tree) lAdj)
	where first = getFirst l 1; nova = go l (Tree tree) first; lAdj = ative l nova;


filterTree :: Ord t => (t->Bool) -> Tree t -> [Tree t]
filterTree f Nil = []
filterTree f (Tree tree) = getAll (Tree tree) l
	where l = [f a | a <- tree ]

















