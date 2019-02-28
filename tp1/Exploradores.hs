module Exploradores (Explorador, AB(Nil,Bin), RoseTree(Rose), foldNat, foldRT, foldAB, expNulo, expId, expHijosRT, expHijosAB, expTail, ifExp, singletons, sufijos, inorder, preorder, postorder, dfsRT, ramasRT, hojasRT, listasQueSuman, listasDeLongitud, (<.>), (<^>), (<++>), (<*>)) where

import Prelude hiding ((<*>))

--Definiciones de tipos

type Explorador a b = a -> [b]

data AB a = Nil | Bin (AB a) a (AB a) deriving Eq

data RoseTree a = Rose a [RoseTree a] deriving Eq

-- Definiciones de Show

instance Show a => Show (RoseTree a) where
    show x = concatMap (++"\n") (padTree 0 x)

padTree :: Show a => Int -> RoseTree a -> [String]
padTree i (Rose x ys) =  ((pad i) ++  (show x) ) : (concatMap (padTree (i + 4)) ys)

pad :: Int -> String
pad i = replicate i ' '


instance Show a => Show (AB a) where
  show = padAB 0 0
  
padAB _ _ Nil = ""
padAB n base (Bin i x d) = pad n ++ show x ++ padAB 4 (base+l) i ++ "\n" ++ padAB (n+4+base+l) base d where l = length $ show x


--Ejercicio 1
expNulo :: Explorador a b
expNulo _ = []

expId :: Explorador a a
expId x = [x]

expHijosRT :: Explorador (RoseTree a) (RoseTree a)
expHijosRT (Rose root children) = children

expHijosAB :: Explorador (AB a) (AB a)
expHijosAB Nil = []
expHijosAB (Bin left root right) = [left, right]

expTail :: Explorador [a] a
expTail [] = []
expTail (x:xs) = xs

--Ejercicio 2
foldNat :: (b -> b) -> b -> Integer -> b
foldNat _ _ n | n < 0 = error "Negative number"
foldNat s z 0 = z
foldNat s z n = s (foldNat s z (n-1))

foldRT :: (a -> [b] -> b) -> (RoseTree a) -> b
foldRT f (Rose root children) = f root $ map (\x -> foldRT f x) children

-- f: function to join up results
-- z: neutral element for the function that joins up results
foldAB :: (a -> b -> b -> b) -> b -> AB a -> b
foldAB _ z (Nil) = z
foldAB f z (Bin t1 root t2) = f root (foldAB f z t1) (foldAB f z t2)

--Ejercicio 3
singletons :: Explorador [a] [a]
singletons xs = foldr (\x acc -> [[x]] ++ acc) [] xs

sufijos :: Explorador [a] [a]
sufijos = scanr (\x acc -> x:acc) []

--Ejercicio 4
listasQueSuman :: Explorador Integer [Integer]
listasQueSuman 0 = [[]]
listasQueSuman n = concat [map ((:) (n-i)) (listasQueSuman i) | i <- [0..n-1]] 

binTree = Bin (Bin (Bin (Nil) 1 (Nil)) 2 (Bin (Nil) 3 (Nil))) 4 (Bin (Bin (Nil) 5 (Nil)) 6 (Bin (Nil) 7 (Nil)))

--Ejercicio 5
preorder :: Explorador (AB a) a
preorder = foldAB (\root acc1 acc2 -> root : acc1 ++ acc2) []

inorder :: Explorador (AB a) a
inorder = foldAB (\root acc1 acc2 -> acc1 ++ [root] ++ acc2) []

postorder :: Explorador (AB a) a
postorder = foldAB (\root acc1 acc2 -> acc1 ++ acc2 ++ [root]) []

roseTree = Rose 4 [Rose 2 [Rose 1 [], Rose 3[]], Rose 6 [Rose 5 [], Rose 7 []]]

--Ejercicio 6
dfsRT :: Explorador (RoseTree a) a
dfsRT = foldRT (\root acc -> root:concat acc)

hojasRT :: Explorador (RoseTree a) a
hojasRT = foldRT (\root acc -> if null acc then [root] else concat acc)

ramasRT :: Explorador (RoseTree a) [a]
ramasRT = foldRT (\root acc -> if null acc then [[root]] else map (\path -> root:path) $ concat acc)

--Ejercicio 7
ifExp :: (a->Bool) -> Explorador a b -> Explorador a b -> Explorador a b
ifExp f e1 e2 a = if f a then e1 a else e2 a

--(preorder <++> inorder) binTree

--Ejercicio 8
(<++>) :: Explorador a b -> Explorador a b -> Explorador a b
(<++>) e1 e2 a = e1 a ++ e2 a

--((\x->[1..x]) <.> (map (+1))) [2,4]

--Ejercicio 9
(<.>) :: Explorador b c -> Explorador a b -> Explorador a c
(<.>) e1 e2 a = concat $ map (\x -> e1 x) $ e2 a

--(expHijosAB <^>2) binTree

--Ejercicio 10:t
(<^>) :: Explorador a a -> Integer -> Explorador a a
(<^>) exp n a = foldNat (\acc -> concat $ map exp acc) (exp a) (pred n)



--Ejercicio 11 (implementar al menos una de las dos)
listasDeLongitud :: Explorador Integer [Integer]
listasDeLongitud = (\n -> [ l | s <- [n..], l <- (listasDeLongitudAux n [1..(s-n+1)] s) ])

listasDeLongitudAux :: Integer -> [Integer] -> Integer -> [[Integer]]
listasDeLongitudAux 0 _ _ = [[]]
listasDeLongitudAux 1 _ s = [[s]] 
listasDeLongitudAux n l s = concat [ map ((:) k) (listasDeLongitudAux (n-1) [1..(s-k-n+2)] (s-k)) | k <- l ]

-- expHijosAB <*> binTree

(<*>) :: Explorador a a -> Explorador a [a] 
(<*>) exp xs = takeWhile (not.null) $ iterate (concat.(map exp)) [xs]