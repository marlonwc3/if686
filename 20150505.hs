import Data.Char

-- 1a questao -- 

type Value = String
type Hash = [Value]
type Key = String
type KeyCode = Int

-- Rabin-karp hash
basePrime :: Int 
basePrime = 1009 -- numero primo base

-- parse -- 

getVal :: Char -> Int  
getVal ch = fromEnum(ch) - fromEnum('a') + 1

auxEvaluate :: String -> Int -> Int -> Int 
auxEvaluate "" sum p  = sum
auxEvaluate (h:t) sum p = auxEvaluate t (mod ((mod (27*sum) p) + getVal(h)) p ) p

-- Auxiliares do hash

getEvaluate :: Key -> KeyCode
getEvaluate str = auxEvaluate str 0 basePrime

expand :: Hash -> Int -> Hash
expand hash 0 = hash
expand hash len = expand (hash++("":[])) (len-1)

-- querys

pvt_put :: Hash -> (KeyCode, Value) -> Hash 
pvt_put hash (keyCode, value) 
	| keyCode >= (length hash) = pvt_put (expand hash (keyCode - (length hash) + 300) ) (keyCode,value)
	| otherwise =  (take keyCode hash)++(value:[])++(drop (keyCode+1) hash )

remove :: Hash -> KeyCode -> Maybe Hash 
remove hash keyCode
	| keyCode >= (length hash)  = Nothing
	| otherwise = Just ( pvt_put hash (keyCode, "") )
	
get :: Hash -> KeyCode -> Maybe Value 
get hash keyCode 
	| keyCode >= (length hash) = Nothing
	| otherwise = Just (hash!!keyCode)
	
	
	
	

-- 2a questao --

-- auxiliar
strtok :: String -> String -> [String] -> [String]
strtok [] acc ret = [a | a <-  [acc]++ret, (length a) /= 0]
strtok (h:t) acc ret
	| h == ' ' = strtok t "" (ret++[acc])
	| otherwise = strtok t (acc++[h]) ret
--strtok "MARRLOOON AA A A " "" []

isOkay :: String -> Bool
isOkay [] = True
isOkay (h:t) = (( (h >= 'a') && (h <= 'z')) || ( (h>= 'A') && (h <= 'Z') ) || (h == ' ') ) && (isOkay t	) 
	
upstr :: String -> String 
upstr [] =[] 
upstr (h:t) = [toUpper h]++(upstr t)


process :: String -> Maybe [String]
process a
	| not (isOkay a) = Nothing
	| otherwise = Just (strtok (upstr a) "" [])


printa :: Maybe [String] -> IO ()
printa Nothing = putStr ""
printa (Just []) = putStr ""
printa (Just (h:t) ) = do {
			putStrLn h;
			printa (Just t)
		}

main :: IO () 
main = do 	{
	putStrLn "Entre com a String:";
	line <- getLine;
	linePos <- return (process line);
	printa linePos;
	
	main;
}
























