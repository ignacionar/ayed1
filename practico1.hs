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