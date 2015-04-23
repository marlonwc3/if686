-- TRABALHO 9 - Exercicio 2 
{-
Para encontrar o menor caminho eu utilizei o algoritmo de Belman-ford, que possui
pior performace quando comparado ao dijkstra mas se adapta para detectação de ciclo negativo ( não implementei a detecção ) 
mas a principal vantagem é que possui muito menos linhas de código, 
sendo resumido à 3 for's encadeados e uma array para a resposta. 
sendo sua complexidade: O(V³) 
A ideia do algoritmo se baseia em relaxar todas as arestas V-1 vezes, sendo V o número de vértices.
-}

data Graph t = Empty | Graph [[(Int, Int)]] [t] deriving (Show, Eq)

indexOf :: [Int] -> Int -> Int -> Int 
indexOf [] v p = (-1)
indexOf (h:t) v p 
	| h == v = p
	| otherwise = indexOf t v (p+1) 

adjustAt :: [Int] -> Int -> Int -> [Int] 
adjustAt [] p v = []
adjustAt l p v = (take (p-1) l )++[v]++(drop p l )

--Belman Ford 
go :: [[(Int, Int)]] -> [Int] -> Int -> Int -> Int -> [Int]
go edgeList dist i u j
	| i > v = dist
	| u > v = go edgeList dist (i+1) 1 1
	| j > (length (edgeList!!(u-1))) = go edgeList dist i (u+1) 1
	| otherwise = go edgeList ( adjustAt dist (fst((edgeList!!(u-1))!!(j-1))) ( min (dist!!((fst((edgeList!!(u-1))!!(j-1)))-1)) ((dist!!(u-1)) + (snd((edgeList!!(u-1))!!(j-1))))  ) ) i u (j+1) 
	where v = length edgeList;
	
menorDist ::  Graph Int -> Int -> Int -> Int
menorDist Empty a b = 0
menorDist (Graph edgeList values) a b 
	| (noA <= 0 || noB <= 0 ) = (-1)
--	| otherwise = 101
	| otherwise = (go edgeList (adjustAt dist noA 0) 1 1 1)!!(noB -1)
	where noA = indexOf values a 1; noB = indexOf values b 1; dist = [99999999 | a <- values]


geraFuncaoMenorCaminho :: Graph Int -> (Int -> Int -> Int) 
geraFuncaoMenorCaminho g = (menorDist g )

baseGraph :: Graph Int
baseGraph = Graph [[(2,1)], [(1,1), (3, 1), (4,2)], [(2,1), (4,5)], [(2,2), (3,5)] ] [1,2,3,4]
-- (geraFuncaoMenorCaminho baseGraph) 1 4
-- (geraFuncaoMenorCaminho baseGraph) 4 1
-- (geraFuncaoMenorCaminho baseGraph) 3 2
-- (geraFuncaoMenorCaminho baseGraph) 2 3
-- (geraFuncaoMenorCaminho baseGraph) 3 3 


{-
vi dist(V, INF); 
dist[s] = 0;
fr( i,0, V-1)
	fr(u, 0, V)
		fr(j, 0, adjList[u].size() ) { ii v = adjList[u][j]; dist[v.st] = min(dist[v.st], dist[u] + v.nd);
-}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
