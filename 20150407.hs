type Nome = String
type Idade = Int
data Pessoa = String Int 
data Pessoas = Pessoa Nome Idade
                deriving (Show)

data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area (Circle r) = pi*r*r
area (Rectangle a b) = a*b
	
data Dias = Segunda Int [String] | Terca Int [String] | 	Quarta Int [String] | 	Quinta Int [String] | 	Sexta Int [String] | Sabado | Domingo

fimSemana :: Dias -> Bool
fimSemana Sabado = False
fimSemana Domingo = False
fimSemana (Segunda a b) = True
fimSemana (Terca a b) = True
fimSemana (Quarta a b) = True
fimSemana (Quinta a b) = True
fimSemana (Sexta a b) = True


data Lista t = Nil | Cons t (Lista t) deriving (Show)
 
toLista :: Lista t -> [t]
toLista Nil = []
toLista (Cons t tail) = [t]++(toLista tail)

fromList :: [t] -> Lista t
fromList [] = Nil
fromList (h:t) = Cons h ( fromList t  ) 
 
baseLista :: Lista Int
baseLista = (Cons 9 (Cons 1 (Cons 5 (Cons 3 Nil)))) 

              
data Tree t = NilT | Node t (Tree t) (Tree t) 
              deriving (Eq, Show)

data Expr = Lit Int | Add Expr Expr | Sub Expr Expr
    deriving (Show)

eval :: Expr -> Int
eval (Lit a) = a
eval (Add e1 e2) = eval(e1) + eval(e2)
eval (Sub e1 e2) = eval(e1) - eval(e2)

showExpr :: Expr -> String
showExpr a  = show (eval(a))


depth :: Tree t -> Int
depth NilT = 0
depth (Node t left right) = 1 + ( max (depth left) (depth right) ) 

collapse :: Tree t -> [t]
collapse NilT = []
collapse (Node t left right) = [t]++collapse(left)++collapse(right)

baseTree :: Tree Int
baseTree = Node 1 (Node 2 ( Node 4 (NilT) (NilT)  ) (NilT)  ) ( Node 3 (NilT) (NilT)  )

pvt_bfs :: Eq t => Tree t -> t -> [Tree t] -> Bool
pvt_bfs NilT x [] = False
pvt_bfs NilT x fila = pvt_bfs (head fila) x (drop 1 fila)
pvt_bfs (Node t left right) x queue
    | t == x = True
    | otherwise = pvt_bfs next x popQueue
    where queueAdj = queue++[left]++[right]; next = head queueAdj; popQueue = drop 1 queueAdj;

bfs ::  Eq t => Tree t -> t -> Bool
bfs no x = pvt_bfs no x []

mapTree :: (t -> u) -> Tree t -> Tree u
mapTree f NilT = NilT
mapTree f (Node t left right ) = Node (f t) (mapTree f left) (mapTree f right) 

foo :: Int -> Char
foo x = toEnum(x+fromEnum('a'))







