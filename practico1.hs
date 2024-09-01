-- 1.1 

esCero :: Int -> Bool 

esCero x = x == 0

-- esCero 1 
-- False 

-- esCero 0 
-- True

-- 1.2

esPositivo :: Int -> Bool 

esPositivo x = x > 0 

-- esPositivo 0
-- False

-- esPositivo 1 
-- True

-- 1.3 

esVocal :: Char -> Bool 

esVocal x | x == 'a' = True 
          | x == 'e' = True 
          | x == 'i' = True 
          | x == 'o' = True 
          | x == 'u' = True 
          | otherwise = False 
 
-- esVocal b 
-- False 

-- esVocal a 
-- True 

valorAbsoluto :: Int -> Int 

valorAbsoluto x | x >= 0 = x 
                | x < 0 = -x

-- valorAbsoluto (-1)
-- 1

-- valorAbsoluto 2
-- 2

------------------------------

-- 2.a

paratodo :: [Bool] -> Bool 

paratodo [] = True 
paratodo (x:xs) = x && paratodo xs 

-- paratodo [True, False]
-- False 

-- paratodo [True, True]
-- True

-- 2.b 

sumatoria :: [Int] -> Int 

sumatoria [] = 0 
sumatoria (x:xs) = x + sumatoria xs 

-- sumatoria [1, 3, 2]
-- 6

-- sumatoria [1, 1, 0]
-- 2

-- 2.c

productoria :: [Int] -> Int 

productoria [] = 1 
productoria (x:xs) = x * productoria xs

-- productoria [1, 2, 3]
-- 6

-- productoria [1, 2, 0]
-- 0

-- 2.d 

factorial :: Int -> Int 

factorial 0 = 1 
factorial n = n * factorial (n - 1)

-- factorial 0 
-- 1 

-- factorial 3 
-- 6

-- 2.e

promedio :: [Int] -> Int 

promedio xs = (sumatoria xs) `div` (length xs)

-- promedio [10, 10, 2]
-- 7

-- promedio [6, 8, 4]
-- 6

------------------------------

-- 3.a.a

-- Variables ligadas: i - Int 
-- Variables libres: xs - [Int]

-- 3.a.b 

-- Variables ligadas: i - Int 
-- Variables libres: xs, x - [Int] , Int 

-- 3.a.d 

-- Variables ligadas: i - Int 
-- Variables libres: xs - [Int]

-- 3.b.a 

mayoresQueCero :: [Int] -> Bool 

mayoresQueCero [] = True 
mayoresQueCero (x:xs) = x > 0 && mayoresQueCero xs 

-- mayoresQueCero [2, 1, 3]
-- True 

-- mayoresQueCero [-1, 0, -2]
-- False

-- 3.b.b

igualQue :: [Int] -> Int -> Bool 

igualQue [] n = False 
igualQue (x:xs) n = x == n || igualQue xs n

-- igualQue [2, 3, 4] 1 
-- False 

-- igualQue [2, 3, 4] 4
-- True

-- 3.b.d

darIgualdad :: [Int] -> Int -> Bool 

darIgualdad [] n = True 
darIgualdad (x:xs) n = x == n 

igualAlSiguiente :: [Int] -> Bool 

igualAlSiguiente [] = True 
igualAlSiguiente (x:xs) = darIgualdad xs x && igualAlSiguiente xs

-- igualAlSiguiente [2, 3]
-- False

-- igualAlSiguiente [2, 2]
-- True 

-- 3.c.a 

-- mayoresQueCero [-5, -3, 4, 8]
-- False 

-- igualQue [-5, -3, 4, 8] 5
-- False 

-- igualAlSiguiente [-5, -3, 4, 8]
-- False

-- 3.c.b

-- mayoresQueCero [11, 2, 5, 8]
-- True

-- igualQue [11, 2, 5, 8] 5
-- True 

-- igualAlSiguiente [11, 2, 5, 8]
-- False

------------------------------

-- 4.a.a 

-- Variables ligadas: i - Int 
-- Variables libres: n - Int 

-- 4.a.b 

-- Variables ligadas: i - Int 
-- Variables libres: xs - [Int]

-- 4.a.c 

-- Variables ligadas: i, j - Int, Int 
-- Variables libres: xs, ys - [Int], [Int]

-- 4.a.d 

-- Variables ligadas: i, j - Int - Int 
-- Variables libres: n - Int 

-- 4.b.a 

factorial' :: Int -> Int 

factorial' 0 = 1 
factorial' n = n * factorial (n - 1)

-- factorial' 0
-- 1 

-- factorial' 4 
-- 24

-- 4.b.b 

promedio' :: [Int] -> Float 

promedio' xs = fromIntegral (sumatoria xs) / fromIntegral (length xs)

-- promedio' [3, 4, 1]
-- 2.6666667

-- promedio' [2, 5, 4]
-- 3.6666667

-- 4.b.c

menorA :: [Int] -> [Int] -> Bool 

menorA xs ys = maximum xs < minimum ys

-- menorA [2, 3, 5] [6, 4, 5]
-- False 

-- menorA [2, 3, 5] [6, 6, 7]
-- True 

-- 4.b.d

evaluarFactorizacion :: Int -> Bool 

evaluarFactorizacion n = any (\(i, j) -> i * j == n) [(i, j) | i <- [2 .. n - 1], j <- [2 .. n - 1]] 

-- evaluarFactorizacion 3 
-- False 

-- evaluarFactorizacion 10 
-- True

-- 4.c.a 

-- factorial' 5
-- 120 

-- 4.c.b 

-- promedio' [6, 9, 3, 9, 8]
-- 7.0

-- promedio' [-3, 9, 8]
-- 4.666665

-- 4.c.c 

-- menorA [-3, 9, 8] [6, 7, 8]
-- False

-- 4.c.d

-- evaluarFactorizacion 5
-- False

------------------------------

-- 5. 

todos :: [Bool] -> Bool 

todos [] = True 
todos (x:xs) = x && todos xs

-- todos [True, False, False]
-- False

-- todos [True, True, True]
-- True

------------------------------

-- 6.a.b

-- Variables libres: n, xs - Int, [Int]

-- 6.a.c

-- Variables libres: xs - [Int]

-- 6.a.d

-- Variables libres: xs - [Int]

-- 6.b.b

elMasGrande :: Int -> [Int] -> Bool 

elMasGrande n xs = maximum xs == n 

-- 6.b.c 

productoDePares :: [Int] -> Int 

productoDePares [] = 1
productoDePares (x:xs) | x `mod` 2 == 0 = x * productoDePares xs 
                       | otherwise = productoDePares xs

-- 6.b.d 

comprobarIndice :: Int -> [Int] -> Int 

comprobarIndice i [] = 0 
comprobarIndice i xs | i == length xs = 0
                     | i `mod` 2 == 0 = (xs!!i) + comprobarIndice (i + 1) xs   
                     | i `mod` 2 /= 0 = comprobarIndice (i + 1) xs

sumarSoloIndicesPares :: [Int] -> Int 

sumarSoloIndicesPares xs = comprobarIndice 0 xs 

------------------------------

-- 7.a

paratodo' :: [a] -> (a -> Bool) -> Bool 

paratodo' [] f = True 
paratodo' (x:xs) f = (f x) && paratodo' xs f 

-- 7.b

existe' :: [a] -> (a -> Bool) -> Bool 

existe' [] f = False 
existe' (x:xs) f = (f x) || paratodo' xs f 

-- 7.c

sumatoria' :: [a] -> (a -> Int) -> Int 

sumatoria' [] f = 0 
sumatoria' (x:xs) f = (f x) + sumatoria' xs f 

-- 7.d 

productoria' :: [a] -> (a -> Int) -> Int 

productoria' [] f = 1 
productoria' (x:xs) f = (f x) * productoria' xs f

------------------------------

-- 8. 

-- paratodo' [0, 2, 1] esCero 
-- False 

-- paratodo' [0, 0, 0] esCero 
-- True 

-- paratodo' [-1, 2, 3] esPositivo 
-- False 

-- paratodo' [1, 2, 3] esPositivo
-- True 

-- paratodo' "texto" esVocal 
-- False 

-- paratodo' "aei" esVocal 
-- True 

-- existe' [1, 2, 3] esCero 
-- False 

-- existe' [0, 2, 3] esCero 
-- True 

-- existe' [-1, -2, -3] esPositivo
-- False 

-- existe' [-3, 2, -1] esPositivo
-- True 

-- existe' "rythm" esVocal
-- False 

-- existe' "ritmo" esVocal
-- True 

-- sumatoria' [-1, 5, -2] valorAbsoluto 
-- 8 

-- sumatoria' [-1, -2, 0] valorAbsoluto 
-- 3 

-- productoria' [-1, -2, -3] valorAbsoluto
-- 6

-- productoria' [0, 2, -3] valorAbsoluto
-- 0

------------------------------

-- 9.a. 

-- Podemos definir una función o usar una función lambda

esImpar :: Int -> Bool 

esImpar x = x `mod` 2 /= 0 

todosPares :: [Int] -> Bool 

todosPares xs = length (filter esImpar xs) == 0 

todosPares' :: [Int] -> Bool  

todosPares' xs = length (filter (\x -> x `mod` 2 /= 0) xs) == 0

-- todosPares [1, 3, 5]
-- False 

-- todosPares [4, 6]
-- True

-- 9.b. 

restoCero :: Int -> Int -> Bool 

restoCero z x = z `mod` x == 0 

hayMultiplo :: Int -> [Int] -> Bool 

hayMultiplo z xs = length ( filter ( restoCero z ) xs ) > 0

hayMultiplo' :: Int -> [Int] -> Bool 

hayMultiplo' z xs = length ( filter (\ x -> z `mod` x == 0 ) xs ) > 0

-- hayMultiplo 4 [5, 3]
-- False

-- hayMultiplo 4 [2, 3]
-- True 

-- 9.c.

sumaCuadrados :: Int -> Int 

sumaCuadrados n = sumatoria (map (^2) [0..n])

-- 9.d.

existeDivisor :: Int -> [Int] -> Bool 

existeDivisor n ls = length (filter (restoCero n) ls) > 0

existeDivisor' :: Int -> [Int] -> Bool 

existeDivisor' n ls = length (filter (\ x -> n `mod` x == 0) ls) > 0

-- existeDivisor 9 [10, 5]
-- False 

-- existeDivisor 9 [3, 5]
-- True

-- 9.e.

esPrimo :: Int -> Bool 

esPrimo n = length (filter (restoCero n) [2..(n-1)]) == 0 

esPrimo' :: Int -> Bool 

esPrimo' n = length (filter (\x -> x `mod` n == 0) [2..(n-1)]) == 0

-- esPrimo 4 
-- False 

-- esPrimo 5 
-- True

-- 9.f.

factorial'' :: Int -> Int 

factorial'' n = productoria [1..n]

-- factorial'' 3 
-- 6

-- factorial'' 4
-- 24

-- 9.g. 

multiplicaPrimos :: [Int] -> Int 

multiplicaPrimos xs = productoria ( filter esPrimo xs )

-- multiplicaPrimos [1, 4, 8]
-- 1

-- multiplicaPrimos [4, 5, 11]
-- 55

-- 9.h. 

fib :: Int -> Int 

fib 0 = 0 
fib 1 = 1 
fib n = fib (n - 1) + fib (n - 2)

esFib :: Int -> Bool 

esFib n = length ( filter (\x -> x == n) [0..(fib n)] ) > 0

-- esFib (-1)
-- False 

-- esFib 1
-- True

-- 9.i.

todosFib :: [Int] -> Bool 

todosFib xs = paratodo' xs esFib

-- todosFib (0, 1, (-1))
-- False

-- todosFib [0, 1]
-- True

------------------------------

-- 10.a. 

duplicarValores :: [Int] -> [Int]

duplicarValores [] = []
duplicarValores (x:xs) = (x * 2) : duplicarValores xs

-- 10.b. 

duplicarValores' :: [Int] -> [Int]

duplicarValores' xs = map (* 2) xs

-- duplicarValores [2, 4]
-- [4, 8]

-- duplicarValores' [3, 6]
-- [6, 12]

------------------------------

-- 11.a. 

listaDePrimos :: [Int] -> [Int]

listaDePrimos [] = []
listaDePrimos (x:xs) | esPrimo x = x : listaDePrimos xs 
                     | otherwise = listaDePrimos xs 

-- listaDePrimos [1, 3, 4]
-- [1, 3]

-- listaDePrimos [10, 5, 6]
-- [5]

-- 11.b. 

listaDePrimos' :: [Int] -> [Int]

listaDePrimos' xs = filter esPrimo xs 

-- listaDePrimos' [1, 3, 4]
-- [1, 3]

-- listaDePrimos' [10, 5, 6]
-- [5]

-- 12.a.

primIgualesA :: Eq a => a -> [a] -> [a]

primIgualesA z [] = []
primIgualesA z (x:xs) | z == x = x : primIgualesA z xs 
                      | z /= x = [] 

-- primIgualesA 3 [3, 3, 4, 3]
-- [3, 3]

-- primIgualesA 'a' "aaadaa"
-- "aaa"

-- 12.b. 

primIgualesA' :: Eq a => a -> [a] -> [a]

primIgualesA' z xs = takeWhile (== z) xs

-- primIgualesA' 3 [3, 3, 4, 3]
-- [3, 3]

-- primIgualesA' 'a' "aaadaa"
-- "aaa"

-- 13.a.

primIguales :: Eq a => [a] -> [a]

primIguales [] = []
primIguales xs = takeWhile (== head xs) xs

-- primIguales [4, 3, 3, 4, 1]
-- [4]

-- primIguales "aaadaa"
-- "aaa"

-- 13.b. 

primIguales' :: Eq a => [a] -> [a]

primIguales' [] = []
primIguales' xs = primIgualesA (head xs) xs

-- primIguales' [4, 3, 3, 4, 1]
-- [4]

-- primIguales' "aaadaa"
-- "aaa"