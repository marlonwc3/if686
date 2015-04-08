{- 1ª Questão:  -} 
{-
	Graph [[(Int, Int)]] [t] deriving (Show, Eq)
	[[(Int, Int)]] == lista de adjacência de cada nó 
	[t] == lista dos valores de cada nó
	Eu mapeio cada no, independente do valor do elemento em inteiros pois fica mais facil de trabalhar, se eu fosse criar uma função que insere ela teria um contador que identificaria unicamente cada elemento do do grafo e assinalaria o elemento inserido a esse contador, que no caso é o índice nas duas listas.
-}
data Graph t = Empty | Graph [[(Int, Int)]] [t] deriving (Show, Eq)

baseGraph :: Graph String 
baseGraph = Graph [[(2,5),(3,1), (6,5)], [(4,2)], [(4,3),(5,1)], [(1,2)], [(3,4)], [] , []] ["A", "B", "C", "D", "E", "F", "G"]


{-2ª Questao -}

-- retorna o 1º array de bool com todos os elementos de indices contidos no vetor de int iguais a true (e preserva os antigos).
markAdjs :: [Bool] -> [Int] -> [Bool]
markAdjs mark [] = mark
markAdjs mark (h:t) = markAdjs ((take (h-1) mark)++[True]++(drop h mark)) t 

{-
	bfs que começa sendo executada em 1 elemento, passa na recursao a fila de processamento e um array de todos os nós ja marcados.
-}
pvt_bfs :: Eq t => Show t => Graph t -> t -> [Int] -> [Bool] -> (Bool, [Bool])
pvt_bfs (Graph adjList values) x [] mark = (False, mark)
pvt_bfs (Graph adjList values) x queue mark
	| (values!!(top-1)) == x = (True, mark)
	| otherwise = pvt_bfs (Graph adjList values) x queueAdj (markAdjs mark adjs)
	where top = head queue; popped = (drop 1 queue); adjs = [ fst (a) | a <- adjList!!(top-1), ( not (mark!!(fst(a)-1))  ) ]; queueAdj = popped++adjs; 

{-
	Enquanto houver nós não marcados após execuções de bfs, 
	uma bfs começando a partir desse nó é executada,
	assim eu alcanço todos os componentes conexos.
-}
whileBfs ::  Eq t => Show t => Graph t -> t-> [Bool] -> Int-> Bool
whileBfs (Graph adjList values) x mark y
	| (y > (length adjList) ) = False
	| (mark !! (y-1)) == True = whileBfs (Graph adjList values) x mark (y+1)
	| otherwise = res || whileBfs (Graph adjList values) x markPos (y+1)
	where ret = pvt_bfs (Graph adjList values) x [y] (markAdjs mark [y]); res = fst ret; markPos = snd ret;

-- assinatura da bfs da questao, mais facil de usar.
bfs :: Eq t => Show t => Graph t -> t ->  Bool
bfs (Empty) x = False
bfs (Graph adjList values) x = whileBfs (Graph adjList values) x [False | a <- adjList ] 1

-- ex: bfs baseGraph "X"

 
 
 
 
 
 
 
 
 
