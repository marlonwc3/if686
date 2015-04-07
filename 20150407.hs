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

toLista :: Lista t -> [t]
toLista Nil = []
toLista (Cons t tail) = [t]++(toLista tail)

baseLista :: Lista Int
baseLista = (Cons 9 (Cons 1 (Cons 5 (Cons 3 Nil))))







