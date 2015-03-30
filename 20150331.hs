{- 
1ª) 
O polimorfismo de haskell é mais seguro quanto a garantia de que os tipos terão implementações das funcionalidades que serão utilizadas sob eles, pois é possível restringir que os argumentos genéricos possuam implementações de determinadas funções, sendo esses definidos por uma classe. Em Java, o Generics que é utilizado no polimorfismo acaba sendo mais geral pois não restringe os dados, porém é mais sucetível à erro tendo em vista que apenas ocorre uma verificação de tipo em tempo de execução e depois ocorre um "type erasure" para gerar compatibilidade com as JVM antigas, pois Generics foi adicionada a partir do Java 5.
-}

-- 2ª
getVal :: Char -> Int  
getVal ch = fromEnum(ch) - fromEnum('0')

parseInteger :: String -> Int -> Int
parseInteger [] sum = sum
parseInteger (h:t) sum = parseInteger t (10*sum + getVal(h) )

pvt_getNext :: String -> Char -> Int -> String
pvt_getNext [] last qt = (show qt)++([last])
pvt_getNext (h:t) last qt
	|  (h == last)  = pvt_getNext t last (qt+1)
	| otherwise = (show qt)++([last])++( pvt_getNext t h 1  )
	
getNext :: String -> String 
getNext "" = ""
getNext (h:t) = pvt_getNext t h 1


pvt_fooB :: Int -> String -> Int
pvt_fooB 0 curr = parseInteger curr 0
pvt_fooB n curr = pvt_fooB (n-1) (getNext curr)

foo :: Enum n => n -> Int -- Função que calcula o n-esimo numero, da estouro para n>=10 , uma saída é retornar uma String, como BigInteger de Java
foo n = pvt_fooB (fromEnum(n)-1) "1"

-- 

