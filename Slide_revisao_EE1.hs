	
import System.Random

-- 7 da lista 2 de 2012.2 -- 
data Temperatura = Celsius Float | Farenheit Float | Kelvin Float

f :: Temperatura -> Float
f (Celsius x) = x
f (Farenheit x ) = ((x-32.0)*5.0)/9.0
f (Kelvin x ) = x-273.0

-- deriving (Ord, Eq , Show)
instance Eq Temperatura where
	(==) a b = (f a) == (f b)

instance Ord Temperatura where
	(>) a b = f a > f b
	(>=) a b = f a >= f b
	(<)  a b  = f a < f b
	(<=)  a b = f a <= f b		

instance Show Temperatura where
	show (Celsius x) = (show x) ++ "C"
	show (Farenheit x) = (show x) ++ "F"
	show (Kelvin x) = (show x) ++ "K"			

quicksort :: Ord x => [x] -> [x]
quicksort [] = []
quicksort l = (quicksort [a | a <- l, a < (l!!0) ]) ++ [l!!0] ++ (quicksort [a | a <- (drop 1 l ), a >= (l!!0) ])

minMax :: [Temperatura] -> (Temperatura, Temperatura)
minMax l = ((head l2), (head (reverse l2) ))
	where l2 = quicksort l
	
baseList :: [Temperatura] 
baseList = [(Celsius 100), (Farenheit 180)]

-- questao 1 da lista 2 de 2013.1 -- 

data Elemento = Fogo | Vento | Trovao | Terra | Agua deriving (Show)
data Carta = Card Elemento Int deriving (Show)
data Jogador = Player Int [Carta] (Elemento -> Int)

instance Show Jogador where
	show (Player v l f ) = (show v) ++ " " ++  (show l) 
instance Eq Jogador where
	(==) (Player v l f ) (Player v2 l2 f2 ) = (v==v2)

instance Ord Jogador where
	(>) (Player v l f ) (Player v2 l2 f2 ) = (v>v2)
	(>=) (Player v l f ) (Player v2 l2 f2 ) = (v>=v2)
	(<) (Player v l f ) (Player v2 l2 f2 ) = (v<v2)
	(<=) (Player v l f ) (Player v2 l2 f2 ) = (v<=v2)			

get :: Jogador -> Int
get (Player x ((Card el v):t) f) = v + (f el)


getMenor :: [(Int, Jogador)] -> Int -> Int -> Int
getMenor l menor p 
	| p >= (length l) = menor
	| get (snd (l!!p)) < get (snd (l!!menor) ) = getMenor l p (p+1)
	| otherwise = getMenor l menor (p+1)
	

decc :: [(Int, Jogador)] -> [(Int, Jogador)]
decc [] = []
decc ((v, (Player x l f) ):t) = [(v, (Player x (drop 1 l) f) )]++(decc t)

put :: [t] -> t -> Int -> [t] 
put l v p = (take (p-1) l ) ++ [v] ++ (drop p l)

pvt_partida :: [(Int, Jogador)] -> [(Int, Jogador)]
pvt_partida ((v, (Player  x l f)):t)
	| (length l) == 0 = reverse (quicksort lista)
	| otherwise = pvt_partida nova2
	where lista = ((v, (Player  x l f)):t); menor = getMenor lista 1 0; nova = decc lista; nova2 = put nova ( (fst (nova!!menor) )+1, snd ( nova!!menor )  ) (menor+1)
	
	
partida :: [Jogador] -> [Jogador] 
partida l = [ b  | (a,b) <- (pvt_partida [ (0, a) | a <- l ]) ]

iterChar :: Char -> Int -> Int -> Char
iterChar c p limit
	| p == limit = c
	|otherwise = iterChar (succ c) (p+1) limit
	
isOkay :: String -> Bool
isOkay [] = True
isOkay (h:t) = ((h >= 'a') && (h <= 'z') ) && (isOkay t)

type Path = String

rot :: Char -> Char
rot c = iterChar 'a' 0 (((fromEnum(c)-fromEnum('a'))+13) `mod` 26) 

rot13 :: String -> String
rot13 str = foldr (\ a b -> [rot a]++b ) [] str

mymain :: IO() 
mymain = do {
	putStrLn "Digite o nome do arquivo:";
	w <- getLine;
	s <- readFile w;
	-- putStrLn "Digite 1 para cifrar o documento, e 2 para decifralo:";	
	-- op <- getLine;
	putStrLn "Chegou 1";
	if (not (isOkay (show s))) then do 
		putStrLn "Chegou 2";
		writeFile w "Arquivo invalido";
	else
		writeFile w (rot13 (show (s) ) );

	putStrLn "Operacao finalizada";	
}


