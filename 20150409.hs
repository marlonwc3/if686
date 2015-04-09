-- Questoes da aula

-- Questões da aula

mymap' :: (Int -> Int) -> [Int] -> [Int]
mymap' f [] = []
mymap' f (h:t) = f h : mymap' f t

mysqr :: Int -> Int
mysqr a = a*a

mymap :: (t -> u) -> [t] -> [u] 
mymap f x = [ f a | a<- x ]

pos :: Char -> Int
pos c 
    | fromEnum(c) >= 97 = fromEnum(c) - fromEnum('a') + 1
    | otherwise = fromEnum(c) - fromEnum('A') + 1
    
-- mymap pos "marlon"


foldr' :: ( t -> u -> u ) -> u -> [t] -> u
foldr' f s [] = s
foldr' f s (h:t) = f h (foldr' f s t)

{-
member' :: Eq a => a -> [a] -> Bool
member' = foldr' 
-}




-- TRABALHO

{- 1ª Questão:  -} 
{-
	Graph [[(Int, Int)]] [t] deriving (Show, Eq)
	[[(Int, Int)]] == lista de adjacência de cada nó , fst == adjacente, snd == peso
	[t] == lista dos valores de cada nó
	Eu mapeio cada no, independente do valor do elemento em inteiros pois fica mais facil de
	trabalhar, se eu fosse criar uma função que insere ela teria um contador que identificaria 
	unicamente cada elemento do do grafo e assinalaria o elemento inserido a esse contador, 
	que no caso é o índice nas duas listas.
-}
data Graph t = Empty | Graph [[(Int, Int)]] [t] deriving (Show, Eq)

baseGraph :: Graph String 
--baseGraph = Graph [[(2,3)], []] ["A", "B"]
baseGraph = Graph [[(2,5),(3,1), (6,5)], [(4,2)], [(4,3),(5,1)], [(1,2)], [(3,4)], [] , []] ["A", "B", "C", "D", "E", "F", "G"]


{-2ª Questao -}

{- retorna a 1º lista de bool com todos os elementos de indices contidos na lista de int iguais
   a true (e preserva os antigos).
-}
markAdjs :: [Bool] -> [Int] -> [Bool]
markAdjs mark [] = mark
markAdjs mark (h:t) = markAdjs ((take (h-1) mark)++[True]++(drop h mark)) t 

{-
	dfs que começa sendo executada no elemento no, e tendo conhecimento de quem foi seu pai
	chama a recursao para 1 adjacente,
	quando esse adjacente terminar sua execucao, ele retomara a chamada para seu pai (que eh o no atual)
	eh retornada a nova lista dos nós marcados após sua recursão,
	toda a execução da dfs é feita numa "lista de recursões" encadeadas, pois nessa implementação 
	não ocorre backtracking.
-}
pvt_dfs :: Eq t => Show t => Graph t -> t -> Int -> Int ->  [Bool] -> (Bool, [Bool])
pvt_dfs (Empty) x no pai mark = (False, mark)
pvt_dfs (Graph adjList values) x no pai mark
	| no == -1 = (False, mark)
	| x == (values!!(no-1)) = (True, mark)
	| (length adjs == 0) = pvt_dfs (Graph adjList values) x pai (-1) mark
	| otherwise = pvt_dfs (Graph adjList values) x (head adjs) (no) (markAdjs mark [head adjs])
	where adjs = [ fst a | a <- (adjList!!(no-1)), not (mark!!((fst a)-1)) ];


{-
	Enquanto houver nós não marcados após execuções de dfs, 
	uma dfs começando a partir desse nó é executada,
	caso um nó da chamada atual ja tiver sido marcado antes, entao a recursão é chamada para o próximo nó,
	assim eu itero sob todos os nós e garanto que todos os nós vão ter sido marcados após as execuçõas
	e alcanço todos os componentes conexos, garantindo se o valor em questão está ou não em algum dos
	componentes conexos.
-}	
whileDfs :: Eq t => Show t => Graph t -> t -> Int -> [Bool] -> Bool
whileDfs Empty x no mark = True
whileDfs (Graph adjList values) x no mark
	| no > (length adjList) = False
	| (mark!!(no-1) == True) = whileDfs (Graph adjList values) x (no+1) mark
	| otherwise = res || whileDfs (Graph adjList values) x (no+1) markCall
	where ret = pvt_dfs (Graph adjList values) x no (-1) (markAdjs mark [no]); res = fst ret; markCall = snd ret;

-- assinatura da dfs da questao, mais facil de usar.
dfs :: Eq t => Show t => Graph t -> t -> Bool
dfs (Empty) x = False
dfs (Graph adjList values) x = whileDfs (Graph adjList values) x 1 ([False | a <- adjList])

-- ex: dfs baseGraph "X"




{- 
  Obs: Li rapido o enunciado e pensei que era busca em largura, acabei fazendo primeiro uma bfs para
 resolver a questao, antes de fazer uma dfs, codigo abaixo. 
-}

pvt_bfs :: Eq t => Show t => Graph t -> t -> [Int] -> [Bool] -> (Bool, [Bool])
pvt_bfs (Graph adjList values) x [] mark = (False, mark)
pvt_bfs (Graph adjList values) x queue mark
	| (values!!(top-1)) == x = (True, mark)
	| otherwise = pvt_bfs (Graph adjList values) x queueAdj (markAdjs mark adjs)
	where top = head queue; popped = (drop 1 queue); adjs = [ fst (a) | a <- adjList!!(top-1), ( not (mark!!(fst(a)-1))  ) ]; queueAdj = popped++adjs; 

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

 
 
 
 
 
 
 
 
