--Esercizio 1.1
shuffle :: Eq a => [a] -> [a] ->[a] -> Bool
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


--Esercizio 1.2
-- creo una lista con infiniti zs
    -- inf zs = zs: inf zs
-- uso la zip per zippare ((inf zs) con (genShuffle xs ys)), per lazyness avro tante tuple quante sono  gli element in (genShuffle xs ys)
--uso any (\(zs, xys) -> zs==xys ) listazippata

--implementazione 

inf :: [t] -> [[t]]
inf zs = zs: inf zs

isShuffle :: Eq a => [a] -> [a] ->[a] -> Bool
isShuffle xs ys zs  = any (\(zs, xys) -> zs==xys ) lista where
    lista = zip (inf zs) (genShuffle xs ys) 

--Esercizio 1.3 [Facoltativo]
shufflePlus :: Eq a => [a] -> [a] ->[a] -> Bool
shufflePlus (x:xs) (y:ys) (z:zs)
    | x == z && y == z = True && (shuffle xs (y:ys) zs || shuffle (x:xs) ys zs) 
    | x == z = True && shuffle xs (y:ys) zs
    | y == z = True && shuffle (x:xs) ys zs
    | otherwise = False
shufflePlus (x:xs) [] (z:zs) 
    | x == z = True && shuffle xs [] zs
    | otherwise = False

shufflePlus [] (y:ys)  (z:zs) 
    | y == z = True && shuffle [] ys zs
    | otherwise = False

shufflePlus [] []  (z:zs) = False

shufflePlus [] [] [] = True

shufflePlus _ _ [] = False 

--Esercizio 1.4 [Facoltativo]

xsMenoI :: [a] -> [[a]]
xsMenoI [] = [[]]
xsMenoI (x:xs) = xs : map (x:) (xsMenoI xs)

anagrammi :: [a] -> [[a]]
anagrammi [] = [[]]
anagrammi xs = concat (zipWith (\x -> map (x:)) xs (map anagrammi (xsMenoI xs)) )

--per genereare anche lo shuffle di ys e xs che abbiano elementi in comune basta modificare lo shuffle con shufflePlus
genShuffle :: Eq a => [a] -> [a] -> [[a]]
genShuffle xs ys = filter (shuffle xs ys) anagramma  where
    anagramma = anagrammi (xs++ys)



--Esercizio 2.1
--funzione ausiliaria a segments
loop :: ([a] -> [a]) -> [a] -> [[a]]
loop f xs@(x:txs) = xs : loop f (f xs)
loop f [] = []


segments :: [a] -> [[a]]
segments x = (concat . (map (loop tail)  . ( loop init) ) ) x ++ [[]] 

-- Esercizio 3.1

-- applyL
applyL :: [t -> a] -> [t] -> [a]
applyL (f:fs)(x:xs)= f x : applyL fs xs
applyL _ _ = [] 

myZipWith :: (a1 -> t -> a2) -> [a1] -> [t] -> [a2]
myZipWith f xs ys = applyL (map f xs) ys

--Esercizio 3.2
myMapR :: Foldable t => (a1 -> a2) -> t a1 -> [a2]
myMapR f xs = foldr ((:) . f) [] xs

--Esercizio 3.3
myMapL :: Foldable t => (a1 -> a2) -> t a1 -> [a2]
myMapL f xs = reverse ( foldl (flip ((:) . f) )  []  xs)

--Esercizio 3.4
{-
per implementare foldr e foldl con map, map dovrebbe essere in grado di restituire tutti i risultati(tipi) che foldr e foldl restituiscono,
guardando il tipo di map (map :: (a -> b) -> [a] -> [b]) possiamo intuire che questo non e' possibile in quanto map restituisce sempre 
una lista, mentre con foldr (foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b) possiamo vedere che restituisce NON soltanto liste.
degli esempi possono essere le operazioni che hanno una forma [a]->int, tipo la funzione length oppure quella di somma, con foldr sono 
facilmente implementabili ma con solo map no in quanto resituira' una lista e non un intero, con l'aiuto di funzioni come sum si potrebbe creare,
ma il punto e' che non si potrebbe creare un'unica funzione con map che possa implemetare tutte le operazioni che possono fare foldr e foldl,
ogni volta avremmo bisogno di combinare delle funzioni diverse con map per trasformare(far collassare) la lista in un altro tipo
quindi non si puo implementare foldr/foldl con map
-}


--Esercizio 4.1 [Facoltativo]
part :: (Ord a1, Enum a1, Num a2, Num a1) => a1 -> a2
part n = partAux n  (map (:[]) [1..n])


partAux :: (Ord a1, Enum a1, Num a2, Num a1) => a1 -> [[a1]] -> a2
partAux n [] = 0
partAux n (x:xs)
    |  s == n    = 1
    |  s > n     = r
    |  otherwise =(partAux n  (map (:x) [m..n-1])) + r
        where
            s = sum x
            r = partAux n xs
            m = head x

--Esercizio 4.2 [Facoltativo]
part' :: (Ord a1, Enum a1, Num a2, Num a1) => a1 -> a2
part' n = partAux' n  (map (:[]) [1..n])

partAux' :: (Ord a1, Enum a1, Num a2, Num a1) => a1 -> [[a1]] -> a2
partAux' n [] = 0
partAux' n (x:xs)
    |  s == n    = 1
    |  s > n     = r
    |  otherwise =(partAux' n  (map (:x) [1..n-1])) + r
        where
            s = sum x
            r = partAux' n xs


--Esercizio 4.3 [Facoltativo]
parts :: (Ord a, Enum a, Num a) => a -> [[a]]
parts n = partsAux n  (map (:[]) [1..n])

partsAux :: (Ord a, Enum a, Num a) => a -> [[a]] -> [[a]]
partsAux n [] = []
partsAux n (x:xs)
    |  s == n    = x: r
    |  s > n     = r
    |  otherwise =(partsAux n  (map (:x) [m..n-1])) ++ r
        where
            s = sum x
            r = partsAux n xs
            m = head x

-- Esercizio 4.4 [Facoltativo]
part'' :: (Ord a, Enum a, Num a) => a -> Int
part'' n = length (parts n)

-- la complessita' temporale, asintoticamente e' piu o meno uguale, 
-- mentre la complessita' in termini di spazio, part'' e' sicuramente maggiore di part



