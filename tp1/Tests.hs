module Main where
import Exploradores
import Test.HUnit
import Data.List
import Prelude hiding ((<*>))

--Nota: para poder cargar este archivo es necesario completar todas las declaraciones de tipos que faltan en Exploradores.hs.

-- Evaluar main para correr todos los tests

type Algo = Bool

allTests = test [
  "exploradoresSencillos" ~: testsExpSencillos,
  "exploradoresListas" ~: testsExpListas,
  "exploradoresNaturales" ~: testsExpNaturales,
  "exploradoresAB" ~: testsExpAB,
  "exploradoresRT" ~: testsExpRT,
  "exploradoresCondicional" ~: testsExpCondicional,
  "exploradoresConcatenacion" ~: testsExpConcatenacion,
  "exploradoresComposicion" ~: testsExpComposicion,
  "exploradoresRepeticion" ~: testsExpRepeticion,
  "exploradoresListasDeLongitud" ~: testsExpListasDeLongitud,
  "exploradoresComposicion1" ~: testsExpComposicion1

  ]

testsExpSencillos = test [
    ([] :: [[Algo]]) ~=? expNulo [1, 2, 3, 4, 5, 6],
    ([] :: [[Algo]]) ~=? expNulo ab5,
    [[1, 2, 3, 4, 5, 6]] ~=? expId [1, 2, 3, 4, 5, 6],
    [3] ~=? expId 3,
    [ab1] ~=? expId ab1,
    [rt4] ~=? expId rt4,
    [rt1, rt1] ~=? expHijosRT rt2,
    [rt7, rt8] ~=? expHijosRT rt3,
    [rt9, rt10, rtHoja 4] ~=? expHijosRT rt6,
    [(Rose 2 []),(Rose 3 [(Rose 5 []),(Rose 6 [Rose 7 []])]),(Rose 4 [])] ~=? expHijosRT rt4, 
    [rt9, rt10, rtHoja 4] ~=? expHijosRT rt6,
    [rt1] ~=? expHijosRT rt7,
    [rt2] ~=? expHijosRT rt8,
    [rtHoja 0, rtHoja 5] ~=? expHijosRT rt9,
    [rtHoja 5,rt11] ~=? expHijosRT rt10,
    [rtHoja 7] ~=? expHijosRT rt11,
    [] ~=? expHijosRT rt1,
    [abHoja 4, abHoja 5] ~=? expHijosAB ab1,
    [abNil, abNil] ~=? expHijosAB (abHoja 2),
    [ab6, ab7] ~=? expHijosAB ab5,
    [2,3,4] ~=? expTail [1,2,3,4],
    ([] :: [[Algo]]) ~=? expTail [],
    [] ~=? expTail [1]
  ]


testsExpListas = test [
  ([] :: [[Algo]]) ~=? singletons [],
  [[1], [2], [3], [4], [5] , [6]] ~=? singletons [1, 2, 3, 4, 5, 6],
  [[ab1], [ab2], [ab3], [ab5]] ~=? singletons [ab1, ab2, ab3, ab5],
  [[5], [5], [5]] ~=? take 3 (singletons (repeat 5)),
  ([[]] :: [[Algo]]) ~=? sufijos [],
  sort ["abc", "bc", "c", ""] ~=? sort (sufijos "abc"),
  sort [[8,4,3,2], [4, 3, 2], [3,2], [2], []] ~=? sort (sufijos [8, 4, 3, 2])
  ]

testsExpNaturales = test [
  sort [[1,1], [2]] ~=? sort (listasQueSuman 2),
  sort [[1,1,1],[1,2],[2,1],[3]] ~=? sort (listasQueSuman 3),
  sort [[4],[3,1],[2,2],[2,1,1],[1,3],[1,2,1],[1,1,2],[1,1,1,1]] ~=? sort (listasQueSuman 4),
  [[]] ~=? listasQueSuman 0
  ]

testsExpAB = test [
  [2, 4, 5] ~=? preorder ab1,
  [4,2,1,3,6,5,7] ~=? preorder ab5,
  [4, 2, 5] ~=? inorder ab1,
  [1, 2, 3, 4, 5, 6, 7] ~=? inorder ab5,
  [4, 5, 2] ~=? postorder ab1,
  [1,3,2,5,7,6,4] ~=? postorder ab5
  ]

testsExpRT = test [
  [1,2,3,5,6,7,4] ~=? dfsRT rt4,
  [1, 2, 0, 5, 3, 5, 6, 7, 4] ~=? dfsRT rt6,
  [3,5,6,7] ~=? dfsRT rt10,
  [[1,2],[1,3,5],[1,3,6,7],[1,4]]  ~=? ramasRT rt4,
  [[1,2,0],[1,2,5],[1,3,5],[1,3,6,7],[1,4]] ~=? ramasRT rt6,
  [[1,2],[1,3,5],[1,3,6,7],[1,4]] ~=? ramasRT rt4,
  [2,5,7,4] ~=? hojasRT rt4,
  [0,5,5,7,4] ~=? hojasRT rt6,
  [2,5,7,4] ~=? hojasRT rt4
  ]

testsExpCondicional = test [
    [2] ~=? ifExp even expId expNulo 2,
    [] ~=? ifExp even expId expNulo 3,
    sort [[1,1,1],[1,2],[2,1],[3]] ~=? sort (ifExp (\x-> x < 4) listasQueSuman expNulo 3),
    [] ~=? ifExp (\x-> x < 4) listasQueSuman expNulo 100
    ]

testsExpConcatenacion = test [
  [4,2,1,3,6,5,7,1,2,3,4,5,6,7] ~=? (preorder <++> inorder) ab5,
  sort [[1, 1, 1], [1, 2], [2, 1], [3], [1, 1, 1], [1, 2], [2, 1], [3]] ~=? sort ((listasQueSuman <++> listasQueSuman) 3),
  [1,2,0,5,3,5,6,7,4, 0,5,5,7,4] ~=? (dfsRT <++> hojasRT) rt6,
  sort ["w", "x", "y", "z", "wxyz", "xyz", "yz", "z", ""] ~=? sort ((singletons <++> sufijos) "wxyz"),
  sort ["a", "b", "c", "abc", "bc", "c", ""] ~=? sort ((singletons <++> sufijos) "abc"),
  sort ["d", "e", "de", "e", ""] ~=? sort ((singletons <++> sufijos) "de")
  ]

testsExpComposicion = test [
  [1,2,3,1,2,3,4,5] ~=? ( (\x->[1..x]) <.> (map (+1)) ) [2,4],
  sort [[7],[4],[5],[4],[5],[5]] ~=? sort (( singletons <.> sufijos ) [7,4,5]),
  sort [[1,2],[2],[],[1,3,5],[3,5],[5],[],[1,3,6,7],[3,6,7],[6,7],[7],[],[1,4],[4],[]] ~=? sort (( sufijos <.> ramasRT ) rt4),
  [abHoja 4,abHoja 5, abNil, abNil] ~=? ( expHijosAB <.> expHijosAB ) ab4
  ]

testsExpRepeticion = test [
  [abHoja 1,abHoja 3, abHoja 5, abHoja 7] ~=? (expHijosAB <^>2) ab5,
  [ab1,ab2] ~=? (expHijosAB <^>1) ab3,
  [rtHoja 0, rtHoja 5, rtHoja 5, rt11] ~=? (expHijosRT <^>2) rt6,
  expHijosRT rt6 ~=? (expHijosRT <^>1) rt6,
  expId 32 ~=? (expId <^> 100) 32,
  [[1], [2], [3]] ~=? (singletons <^> 3) [1,2,3],
  sort [[1,2,3],[2,3],[3],[],[2,3],[3],[],[3],[],[],[2,3],[3],[],[3],[],[],[3],[],[],[]] ~=? sort ((sufijos <^> 3) [1,2,3])
  ]

testsExpListasDeLongitud = test [
  all (\x->elem x (listasDeLongitud 2)) [[1,1],[1,2],[2,1],[1,3],[2,2],[3,1]] ~=? True,
  [] ~=? take 0 (listasDeLongitud 3),
  True ~=? elem [9,10,9] (listasDeLongitud 3),
  all (\x->elem x (listasDeLongitud 8)) [[1,1,1,1,1,1,1,1],[1,1,1,1,1,1,1,2],[1,1,1,1,1,1,2,1]] ~=? True,
  [] ~?= take 0 (listasDeLongitud 0),
  all (\x->elem x (listasDeLongitud 1)) [[1],[2],[3],[4],[5],[6],[7],[8],[9],[10]] ~=? True
  ]


testsExpComposicion1 = test [
 [[1],[1],[1],[1],[1],[1]] ~=? take 6 (expId <*> 1),
 [1,6,21] ~=? map length (take 3 $ sufijos<*> [1,2,3,4,5]),
 [abHoja 4, abHoja 5, abHoja 6, abHoja 7] ~=? (last $ init $ expHijosAB <*> ab3),
 (expHijosAB <^>2) ab3  ~=? (last $ init $ expHijosAB <*> ab3)
 ]


--Ejecución de los tests
main :: IO Counts
main = do runTestTT allTests


----------
--Estructuras para pruebas
abHoja x = Bin Nil x Nil
abNil = Nil
ab1 = Bin (abHoja 4) 2 (abHoja 5)
ab2 = Bin (abHoja 6) 3 (abHoja 7)
ab3 = Bin ab1 1 ab2
ab4 = Bin ab1 1 (abHoja 3)
ab5 = Bin ab6 4 ab7
ab6 = Bin (abHoja 1) 2 (abHoja 3)
ab7 = Bin (abHoja 5) 6 (abHoja 7)

rtHoja x = Rose x []
rt1 = rtHoja 1
rt2 = Rose 0 [rt1, rt1]
rt3 = Rose 1 [rt7, rt8]
rt4 = Rose 1 [(Rose 2 []),(Rose 3 [(Rose 5 []),(Rose 6 [Rose 7 []])]),(Rose 4 [])]
rt5 = Rose () $ replicate 4 rt5
rt6 = Rose 1 [rt9, rt10, rtHoja 4]
rt7 = Rose 2 [rt1]
rt8 = Rose 3 [rt2]
rt9 = Rose 2 [rtHoja 0, rtHoja 5]
rt10 = Rose 3 [rtHoja 5,rt11]
rt11 = Rose 6 [rtHoja 7]


--
--Ejemplos:
--take 10 $ expId <*> 1 ~> [[1],[1],[1],[1],[1],[1],[1],[1],[1],[1]]
--take 3 $ sufijos<*> [1,2,3,4,5] ~>
--[[[1,2,3,4,5]],[[1,2,3,4,5],[2,3,4,5],[3,4,5],[4,5],[5],[]],[[1,2,3,4,5],[2,3,4,5],[3,4,5],[4,5],[5],[],[2,3,4,5],[3,4,5],[4,5],[5],[],[3,4,5],[4,5],[5],[],[4,5],[5],[],[5],[],[]]]
--expHijosAB <*> ab5
--last $ init $ expHijosAB <*> ab3 da las 4 hojas con raíces 4, 5, 6, 7.
--(expHijosAB <^>2) ab3 es equivalente a lo anterior.
--expHijosRT <*> rt4
--(takeWhileAB (< 5) ab3) == (Bin (Bin (Bin Nil 4 Nil) 2 Nil) 1 (Bin Nil 3 Nil))
--dfsRT rt4 ~> [1,2,3,5,6,7,4]
--inorder ab3 ~> [4,2,5,1,6,3,7]
--inorder ab5 ~> [1,2,3,4,5,6,7]
--dfsRT $ head $ truncarRT rt4 ~> [1,3,6]
--take 10 $ aplicacionesInfinitas (+) 1 ~> [1,2,3,4,5,6,7,8,9,10]
--take 3 $ aplicacionesInfinitas (flip podarNivel) rt5