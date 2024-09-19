-- 1.a.

data TipoDeLeche = Descremada | Entera | Condensada | Polvo deriving (Show, Eq)
data UsoDeLeche = Bebida | Preparaciones deriving (Show, Eq)

data Corte = Bife | Molida | Pulpa deriving (Show, Eq)

data TipoDeQueso = Barra | Cremoso | Duro deriving (Show, Eq)

type Peso = Float 
type Precio = Int 

data Perecedero = Leche TipoDeLeche UsoDeLeche Precio | Carne Corte Peso Precio | Queso TipoDeQueso Peso Precio deriving (Show)

-- 1.b.

cuantosQuesos :: [Perecedero] -> TipoDeQueso -> Int 

cuantosQuesos [] q = 0 

cuantosQuesos ((Queso tipoqueso peso precio) : lp) q | q == tipoqueso = 1 + cuantosQuesos lp q 
                                                     | otherwise = cuantosQuesos lp q 
cuantosQuesos (_ : lp) q = cuantosQuesos lp q

-- cuantosQuesos [(Queso Cremoso 1 2), (Leche Descremada Bebida 2), (Queso Cremoso 1 2)] Cremoso 
-- 2

-- 1.c.

instance Eq Perecedero 
  where 
    (Leche t1 u1 p1) == (Leche t2 u2 p2) = (t1 == t2) && (u1 == u2) && (p1 == p2)
    (Carne c1 pes1 prec1) == (Carne c2 pes2 prec2) = (c1 == c2) && (pes1 == pes2) && (prec1 == prec2)

-- 2.a.

data Division = Septima | Sexta | Quinta | Cuarta deriving (Show, Eq)

data NotasDelClub = NoHayMasJugadores | EvolucionDelJugador String Division Int Int Int NotasDelClub deriving (Show, Eq)

-- 2.b.

validarJugadorEnQuinta :: Division -> Int -> Int -> Int -> Bool 

validarJugadorEnQuinta division defensa ataque pases | division == Quinta = defensa >= 7 && ataque >= 7 && pases >= 8
                                                     | otherwise = True 

validarJugadorEnSextaSeptima :: Division -> Int -> Int -> Int -> Bool 

validarJugadorEnSextaSeptima division defensa ataque pases | division == Sexta || division == Septima = (ataque >= 7 || defensa >= 7) && pases >= 6 
                                                           | otherwise = True 

pasaDeDivision :: NotasDelClub -> String -> Bool 

pasaDeDivision NoHayMasJugadores nombre = False 
pasaDeDivision (EvolucionDelJugador n division n1 n2 n3 notasclub) nombre | n == nombre && (validarJugadorEnQuinta division n1 n2 n3) && (validarJugadorEnSextaSeptima division n1 n2 n3) = True
                                                                                | otherwise = pasaDeDivision notasclub nombre

-- 2.c. 

devolverDivision :: NotasDelClub -> String -> Maybe Division

devolverDivision NoHayMasJugadores nombre = Nothing 

devolverDivision (EvolucionDelJugador n d n1 n2 n3 notasclub) nombre | n == nombre = Just d
                                                                            | otherwise = devolverDivision notasclub nombre