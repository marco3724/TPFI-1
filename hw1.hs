-- Esercizio 1.1
shuffle (x:xs) (y:ys) (z:zs)  
    | x == z = True && shuffle xs (y:ys) zs
    | y == z = True && shuffle (x:xs) ys zs
    | otherwise = False

shuffle (x:xs) [] (z:zs) 
    | x == z = True && shuffle xs [] zs
    | otherwise = False

shuffle [] (y:ys)  (z:zs) 
    | y == z = True && shuffle [] ys zs
    | otherwise = False

shuffle [] []  (z:zs) = False

shuffle [] [] [] = True

shuffle _ _ [] = False 
{-

*--Main> shufflePlus [1,1,2] [1,2] [1,2,1,1,2]
True   crea 2 rami 
*Main> shufflePlus [1,1,2] [1,2] [1,1,1,2,2]
True
*Main> shuffle [1,1,2] [1,2] [1,1,1,2,2]    
True
*--Main> shuffle [1,1,2] [1,2] [1,2,1,1,2]
False
-}

--Esercizio 1.2
-- uso la zip per zippare (zs con (genShuffle xs ys)) 
--filtro con  filter (\(zs, xys) -> zs==xys )
-- e faccio any



-- Esercizio 2.1
lol (x:xs) = [x] : lol xs
lol [] = []

suffissi xs@(_:txs) = xs:suffissi txs
suffissi [] = [] 

prefissi xs@(_:txs) = xs:prefissi (init xs)
prefissi [] = [] 

-- segments [] =[[]]
-- segments txs@(x:xs) =      suffissi txs ++ prefissi txs  ++ lol txs


myInit [] = []
myInit xs@(x:txs) = [xs]  : myInit txs 

-- segments xs@(x:txs)= [[l]|l<-]
-- segmentsAux [] = []
-- segmentsAux (x:xs) = [x]: segmentsAux xs



loop f xs@(x:txs) = xs : loop f (f xs)
loop f [] = []

--fun x =takeWhile (not . null)  (loop init x) ++takeWhile (not . null)  (loop tail x) ++[]

--mappo tutti gli elementi di un array[[[x],[x]],[[x]]] e poi concateno

fun  x= (concat . (map (loop tail)  . ( loop init) ) ) x ++ [[]] 

-- init coda coda coda , init coda ...
-- senza map [[[1,2,3],[1,2],[1]],[[1,2],[1]],[[1]]]
-- con map [[[1,2,3],[2,3],[3]],[[1,2],[2]],[[1]]]
-- Esercizio 3.1

-- applyL
applyL (f:fs)(x:xs)= f x : applyL fs xs
applyL _ _ = [] 
--prende un array di funzioni A(che orendon un solo argomento) e un arrayB e restituisce un unico array 
--applica la funzione dell array  di funzione A all'array B

myZipWith f xs ys = applyL (map f xs) ys


--esercizio 3.2
myMapR f xs = foldr ((:) . f) [] xs

--esercizio 3.3
myMapL f xs =  foldl (flip ((:) . f) )  [] (reverse xs)

f xs = xs : f xs

--esercizio 3.4 

--Esercizio 1.4
xsMenoI [] = [[]]
xsMenoI (x:xs) = xs : map (x:) (xsMenoI xs)

anagrammi [] = [[]]
anagrammi xs = concat (zipWith (\x -> map (x:)) xs (map anagrammi (xsMenoI xs)) )

genShuffle xs ys = filter (shuffle xs ys) anagramma  where
    anagramma = anagrammi (xs++ys)



--Esercizio 4.3


parts n = partsAux n  (map (:[]) [1..n])
-- part 4 = partAux 4 [[1],[2],[3],[4]]
partsAux n [] = []
partsAux n (x:xs)
    |  s == n    = x: r
    |  s > n     = r
    |  otherwise = foldr (:) (partsAux n  (map (:x) [h..n-1])) r
        where
            h = head x
            s = sum x
            r = partsAux n xs
--e' inutile che controlla anche n quindi si fa n-1 in quanto la partizione con [n] gia ce dall inzio
-- partaux intero lista di liste
--  se s == n aggiyungo la lista la mia lista di liste
--  se e' maggiore allora restituisco la lista di liste
--  altrimenti faccio il partsaux di n e una lista di liste dove aggiungo h,h+1..,n-1 a x(che e' una lista che ancora non ha la somma come n)
--     e aggiugno ogni elemento in r in  ((partsAux n  (map (:x) [h..n-1]))

--     partaux restituisce una lista di liste


-- notSame [x ,y]= x/=y
-- notSame (x:xs) = x /= (head xs) && notSame xs

-- notOne (x:xs) = length (x:xs) /= 1

-- partOrd n = concat (map anagrammi ( (filter notSame  ( filter notOne (parts n)) ) ))



{-\
*Main> parts 4      
[[1,1,1,1],[2,1,1],[3,1],[2,2],[4]]
*Main> part' 4
8
*Main> part 4 
5
*Main> part'' 4
5
*Main> part'' 8
22
*Main> part' 8 
128
*Main> part' 8
128
*Main> part 8 
22
-}