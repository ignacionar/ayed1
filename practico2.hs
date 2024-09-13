-- 1.a.

sum_cuad :: [Int] -> Int 

sum_cuad [] = 0
sum_cuad (x:xs) = x * x + sum_cuad xs

-- sum_cuad [2, 2]
-- 8

-- sum_cuad [2, 3, 4]
-- 29

-- 1.b.
 
iga :: Eq a => a -> [a] -> Bool 

iga e [] = True 
iga e (x:xs) = e == x && iga e xs 

-- iga 2 [2, 3]
-- False 

-- iga 2 [2, 2]
-- True 

-- 1.c. 

expn :: Num a => a -> Int -> a

expn x 0 = 1 
expn x n = x * expn x (n - 1)

-- expn 2 2 
-- 4 

-- expn 2 3 
-- 8

-- 1.d. 

sum_par :: Int -> Int 

sum_par 0 = 0 
sum_par n | n `mod` 2 == 0 = n + sum_par (n - 1)
          | otherwise = sum_par (n - 1)

-- sum_par 4 
-- 6 

-- sum_par 8
-- 20

-- 1.e. 

cuantos :: (a -> Bool) -> [a] -> Int 

cuantos p [] = 0
cuantos p (x:xs) | p x = 1 + cuantos p xs
                 | otherwise = cuantos p xs

-- cuantos (== 0) [1, 2, 0]
-- 1 

-- cuantos (\x -> x `mod` 2 == 0) [2, 4]
-- 2

------------------------------

-- 2.a. 

data Carrera = Matematica | Fisica | Computacion | Astronomia 

-- 2.b.

titulo :: Carrera -> String 

titulo Matematica = "Lic. en Matemática"
titulo Fisica = "Lic. en Física"
titulo Computacion = "Lic. en Computacion"
titulo Astronomia = "Lic. en Astronomia"

-- 2.c.

-- data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si 

-- 2.d.

cifradoAmericano :: NotaBasica -> Char 

cifradoAmericano Do = 'C' 
cifradoAmericano Re = 'D' 
cifradoAmericano Mi = 'E' 
cifradoAmericano Fa = 'F' 
cifradoAmericano Sol = 'G' 
cifradoAmericano La = 'A' 
cifradoAmericano Si = 'B' 

------------------------------

-- 3.

-- data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si deriving (Eq, Ord)

------------------------------

-- 4.a. 

minimoElemento :: Ord a => [a] -> a 

minimoElemento [x] = x
minimoElemento (x:xs) = min x (minimoElemento xs)

-- 4.b. 

minimoElemento' :: (Bounded a, Ord a) => [a] -> a

minimoElemento' [] = maxBound 
minimoElemento' (x:xs) = x `min` (minimoElemento' xs)

-- 4.c.

data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si deriving (Eq, Ord, Show)

-- minimoElemento [Fa, La, Sol, Re, Fa]
-- Re

------------------------------

-- 5.a.

type Altura = Int 
type NumCamiseta = Int 

data Zona = Arco | Defensa | Mediocampo | Delantera deriving (Eq, Show)
data TipoReves = DosManos | UnaMano deriving (Eq, Show)
data Modalidad = Carretera | Pista | Monte | Bmx  deriving (Eq, Show)

data PiernaHabil = Izquierda | Derecha  deriving (Eq, Show)
type ManoHabil = PiernaHabil

data Deportista = Ajedrecista 
                | Ciclista Modalidad 
                | Velocista Altura 
                | Tenista TipoReves ManoHabil Altura
                | Futbolista Zona NumCamiseta PiernaHabil Altura deriving (Eq, Show)

-- 5.b.

-- :t Ciclista
-- Ciclista :: Modalidad -> Deportista 

-- 5.c.

contar_velocistas :: [Deportista] -> Int 

contar_velocistas [] = 0
contar_velocistas (Velocista x : xs) = 1 + contar_velocistas xs 
contar_velocistas (_ : xs) = contar_velocistas xs

-- contar_velocistas [Velocista 2, Velocista 3, Ciclista Carretera]
-- 2 

-- 5.d. 

contar_futbolistas :: [Deportista] -> Zona -> Int 

contar_futbolistas [] z = 0 
contar_futbolistas ((Futbolista Arco n p a): xs) Arco = 1 + contar_futbolistas xs Arco
contar_futbolistas ((Futbolista Defensa n p a): xs) Defensa = 1 + contar_futbolistas xs Defensa
contar_futbolistas ((Futbolista Mediocampo n p a): xs) Mediocampo = 1 + contar_futbolistas xs Mediocampo
contar_futbolistas ((Futbolista Delantera n p a): xs) Delantera = 1 + contar_futbolistas xs Delantera
contar_futbolistas (_ : xs) z = contar_futbolistas xs z

-- contar_futbolistas [Futbolista Arco 1 Izquierda 2, Futbolista Delantera 1 Izquierda 2, Futbolista Mediocampo 1 Izquierda 2] Mediocampo
-- 1

-- 5.e. 

comparar_zonas :: Zona -> Deportista -> Bool 

comparar_zonas z' (Futbolista z n p a) = z == z' 

contar_futbolistas' :: [Deportista] -> Zona -> Int 

contar_futbolistas' xs z = length (filter (comparar_zonas z) xs)

-- 6.a

f :: Num a => a -> Int -> a 

f x 0 = 0
f x n = f x (n - 1) + expn x n

-- 6.b.

pi2 :: Int -> Int 

pi2 0 = 0 
pi2 n = (pi2 (n - 1)) + (expn (-1) (n - 1)) `div` (2 * (n - 1) + 1)

pi' :: Int -> Int

pi' n = 4 * pi2 n

-- 6.c. 

multiplicar_con_suma :: Int -> Int -> Int 

multiplicar_con_suma x 0 = 0 
multiplicar_con_suma x n = x + multiplicar_con_suma x (n - 1)

f' :: Int -> Int 

f' x = multiplicar_con_suma x (multiplicar_con_suma x x) 

------------------------------

-- 8.a.

comprobar_anterior :: Int -> Int -> Bool 

comprobar_anterior 0 n = False 
comprobar_anterior x n | x * x /= n = comprobar_anterior (x - 1) n 
                       | otherwise = True 

cuad :: Int -> Bool 

cuad 0 = False 
cuad n = comprobar_anterior n n 
       
-- 8.b.

-- 10.a.

sonidoNatural :: NotaBasica -> Int 

sonidoNatural Do = 0 
sonidoNatural Re = 2 
sonidoNatural Mi = 4 
sonidoNatural Fa = 5 
sonidoNatural Sol = 7 
sonidoNatural La = 9 
sonidoNatural Si = 11

-- 10.b.

data Alteracion = Bemol | Natural | Sostenido deriving (Show, Eq)

-- 10.c.

data NotaMusical = Nota NotaBasica Alteracion deriving (Show)

-- 10.e.

sonidoCromatico :: NotaMusical -> Int 

sonidoCromatico (Nota notab alt) | alt == Sostenido = (sonidoNatural notab) + 1 
                          | alt == Bemol = (sonidoNatural notab) - 1
                          | alt == Natural = sonidoNatural notab

-- 10.f.

instance Eq NotaMusical
  where
    n1 == n2 = sonidoCromatico n1 == sonidoCromatico n2

-- 10.g.

instance Ord NotaMusical
  where
    n1 <= n2 = sonidoCromatico n1 <= sonidoCromatico n2

-- 11.a.

primerElemento :: [a] -> Maybe a 

primerElemento [] = Nothing
primerElemento (x:xs) = Just x

-- 12.a.

data Cola = VaciaC | Encolada Deportista Cola 

atender :: Cola -> Maybe Cola 

atender VaciaC = Nothing 
atender (Encolada d c) = Just c

-- 12.b. 

encolar :: Deportista -> Cola -> Cola 

encolar d VaciaC = Encolada d VaciaC
encolar d (Encolada d' c) = Encolada d' (encolar d c)

-- 12.c.

busca :: Cola -> Zona -> Maybe Deportista 

busca VaciaC z = Nothing 
busca (Encolada (Futbolista z' n p a) c) z 
  | z == z' = Just (Futbolista z' n p a)
  | otherwise = busca c z

------------------------------

-- 13.

data ListaAsoc a b = Vacia | Nodo a b (ListaAsoc a b) deriving (Show)

type Diccionario = ListaAsoc String String 
type Padron = ListaAsoc Int String 
type GuiaTelefonica = ListaAsoc Int String

-- 13.a.

la_long :: ListaAsoc a b -> Int 

la_long Vacia = 0 
la_long (Nodo a b l) = 1 + la_long l

-- la_long (Nodo (1::Int) (2::Int) (Nodo (3::Int) (4::Int) Vacia))
-- 2

-- 13.b. 

la_concat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b 

la_concat Vacia l' = l' 
la_concat (Nodo a b l) l' = Nodo a b (la_concat l l')

-- 13.c.

la_agregar :: Eq a => ListaAsoc a b -> a -> b -> ListaAsoc a b

la_agregar Vacia a b = Nodo a b Vacia
la_agregar (Nodo a b la) a' b'
  | a == a' = Nodo a b' la
  | otherwise = Nodo a b (la_agregar la a' b')

-- 13.d.

la_pares :: ListaAsoc a b -> [(a, b)]

la_pares Vacia = []
la_pares (Nodo a b la) = (a, b):la_pares la

-- 13.e.

la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b
la_busca Vacia a = Nothing
la_busca (Nodo a b la) a'
  | a == a' = Just b
  | otherwise = la_busca la a'

-- 13.f.

la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b
la_borrar a Vacia = Vacia
la_borrar a' (Nodo a b la)
  | a' == a = la
  | otherwise = Nodo a b (la_borrar a' la)