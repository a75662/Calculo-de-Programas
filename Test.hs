--
-- Projecto CP 2015/16
--
-- O projecto consiste em desenvolver testes para o módulo Graph.hs
-- (para grafos orientados e não pesados).
-- Mais concretamente, o projecto consiste em 3 tarefas que são descritas abaixo.
-- O prazo para entrega é o dia 3 de Abril. Cada grupo deve enviar apenas
-- o módulo de testes (este módulo) por email para calculodeprogramas@gmail.com
-- O nome do ficheiro deve identificar os números dos 2 alunos do grupo (numero1_numero2.hs).
-- Certifiquem-se que o módulo de testes desenvolvido compila correctamente antes
-- de submeter. O módulo Graph.hs não deve ser alterado.
-- Os 2 alunos do grupo devem também indentificar-se nos comentários abaixo.
--
-- Aluno 1
-- Número: A75662
-- Nome: Fábio Luís Baião da Silva
-- Curso: MiEI
--
-- Aluno 2
-- Número: A74601
-- Nome: José Miguel Ribeiro da Silva
-- Curso: MiEI
--

module Main where

import Graph
import Test.HUnit hiding (path)
import Test.QuickCheck
import Data.Set as Set
import Data.Maybe
--
-- Teste unitário
--

e1 :: Edge Int
e1 = Edge { 
    source = 1,
    target = 2
}

-- Grafo vazio
g0 :: Graph Int
g0 = Graph {
    nodes = fromList [],
    edges = fromList []
}

g1 :: Graph Int
g1 = Graph {
    nodes = fromList [1],
    edges = fromList [Edge 1 1]
}

-- Grafo cíclico
g2 :: Graph Int
g2 = Graph { 
    nodes = fromList [1,2,3],
    edges = fromList [Edge 1 2, Edge 2 3, Edge 3 1]
}

-- Grafo com as arestas invertidas a partir do g2
g2trans :: Graph Int
g2trans = Graph {
    nodes = fromList [1,2,3],
    edges = fromList [Edge 2 1, Edge 3 2, Edge 1 3]
}

-- Floresta a partir do grafo g2 e do vértice 1
g2forest1 :: Graph Int
g2forest1 = Graph {
    nodes = fromList [1,2,3],
    edges = fromList [Edge 2 1, Edge 3 2]
}

-- Grafo inválido
g3 :: Graph Int
g3 = Graph {
    nodes = fromList [1,2,3],
    edges = fromList [Edge 1 2, Edge 2 3, Edge 1 4, Edge 4 2]
}

g4 :: Graph Int
g4 = Graph {
    nodes = fromList [1,2,3],
    edges = fromList [Edge 1 2, Edge 1 3]
}
-- Grafo com as arestas invertidas a partir do g4
g4trans :: Graph Int
g4trans = Graph {
    nodes = fromList [1,2,3],
    edges = fromList [Edge 2 1, Edge 3 1]
}

-- Floresta a partir do grafo g4 e do vértice 1
g4forest1 :: Graph Int
g4forest1 = Graph {
 
    nodes = fromList [1,2,3], 
    edges = fromList [Edge 2 1, Edge 3 1]
}

-- Grafo acíclico
g5 :: Graph Int
g5 = Graph {
    nodes = fromList [1,2,3],
    edges = fromList [Edge 1 3, Edge 2 3]
}

-- Grafo com as arestas invertidas a partir do g5
g5trans :: Graph Int
g5trans = Graph {
    nodes = fromList [1,2,3],
    edges = fromList [Edge 3 1, Edge 3 2]
}

g6 :: Graph Int
g6 = Graph {
  nodes = fromList [1,2,3,4],
  edges = fromList [Edge 1 2, Edge 2 3, Edge 1 3, Edge 4 1]
}

g2_6 :: Graph Int
g2_6 = Graph {
  nodes = fromList [1,2,3,4],
  edges = fromList [Edge 1 2, Edge 2 3, Edge 3 1, Edge 1 3, Edge 4 1]
}

g7 :: Graph Int
g7 = Graph {
    nodes = fromList [1,2,3,4,5,6,7,8,9],
    edges = fromList [Edge 1 2, Edge 1 3, Edge 1 4, Edge 2 4, 
                    Edge 4 5, Edge 4 6, Edge 6 3, Edge 5 7, 
                    Edge 7 8, Edge 8 2, Edge 9 3, Edge 9 6, Edge 9 7]
}

g7forest :: Forest Int
g7forest = Graph {
    nodes = fromList [1,2,3,4,5,6,7,8],
    edges = fromList [Edge 1 2, Edge 1 3, Edge 1 4, Edge 2 4, 
                    Edge 4 5, Edge 4 6, Edge 5 7, Edge 7 8]
}

g8 :: DAG Int
g8 = Graph {
    nodes = fromList [1,2,3,4,5,6,7],
    edges = fromList [Edge 1 2, Edge 2 5, Edge 5 3, Edge 3 7, Edge 3 6, Edge 7 4]
}

g9 :: Graph Int
g9 = Graph {
    nodes = fromList [1,2,3,4,5,6],
    edges = fromList [Edge 1 2, Edge 2 2, Edge 2 4, Edge 2 5, 
                    Edge 4 1, Edge 4 5, Edge 5 4, Edge 6 3]
}

-- Um exemplo de um teste unitário.
test_adj :: Test
test_adj = adj g1 1 ~?= fromList [Edge 1 1]


--
-- Tarefa 1
--
-- Defina testes unitários para todas as funções do módulo Graph,
-- tentando obter o máximo de cobertura de expressões, condições, etc.
--

test_swap :: Test
test_swap = swap e1 ~?= Edge 2 1

test_empty :: Test
test_empty = Graph.empty ~?= g0

testL_isEmpty = TestList  [
    isEmpty g0 ~?= True,
    isEmpty g1 ~?= False,
    isEmpty g2 ~?= False
    ]

testL_isValid = TestList [
    isValid g0 ~?= True,
    isValid g2 ~?= True,
    isValid g3 ~?= False
    ]

testL_isDAG = TestList [
    isDAG g0 ~?= True,
    isDAG g1 ~?= False,
    isDAG g2 ~?= False,
    isDAG g4 ~?= True,
    isDAG g5 ~?= True
    ]

testL_isForest = TestList [
    isForest g0 ~?= True,
    isForest g4 ~?= False,
    isForest g5 ~?= True
    ]

testL_isSubgraphOf = TestList [
    isSubgraphOf g0 g6 ~?= True,
    isSubgraphOf g5 g6 ~?= True,
    isSubgraphOf g6 g5 ~?= False
    ]

testL_adj = TestList [
    test_adj,
    adj g6 2 ~?= fromList [Edge 2 3],
    adj g6 1 ~?= fromList [Edge 1 3, Edge 1 2],
    adj g6 3 ~?= fromList []
    ]

testL_transpose = TestList [
    transpose g0 ~?= g0,
    transpose g2 ~?= g2trans,
    transpose g4 ~?= g4trans,
    transpose g5 ~?= g5trans
    ]

testL_union = TestList [
    Graph.union g6 g0 ~?= g6,
    Graph.union g2 g6 ~?= g2_6,
    Graph.union g6 g2 ~?= g2_6
    ]

testL_bft = TestList [
    bft g0 (fromList []) ~?= g0,
    bft g2 (singleton 1) ~?= g2forest1,
    bft g4 (singleton 1) ~?= g4forest1
    ]

testL_reachable = TestList [
    reachable g6 1 ~?= fromList [1,2,3],
    reachable g6 3 ~?= fromList [3],
    reachable g5 1 ~?= fromList [1,3]
    ]

testL_isPathOf = TestList [
    isPathOf [] g0 ~?= True,
    isPathOf [Edge 4 1, Edge 1 2, Edge 2 3] g6 ~?= True,
    isPathOf [Edge 4 1, Edge 1 3, Edge 3 2] g6 ~?= False
    ]

testL_path = TestList [
    path g6 4 3 ~?= Just [Edge 4 1, Edge 1 3],
    path g5 1 2 ~?= Nothing,
    path g6 2 1 ~?= Nothing,
    path g7 1 7 ~?= Just [Edge 1 4, Edge 4 5, Edge 5 7]
    ]

testL_topo = TestList [
    topo g5 ~?= [fromList [1,2], fromList [3]],
    topo g8 ~?= [fromList [1], fromList [2], fromList [5], 
                fromList [3], fromList [7,6], fromList [4]]
    ]

main = runTestTT $ TestList [
                            test_swap,
                            test_empty,
                            testL_isEmpty,
                            testL_isValid,
                            testL_isDAG,
                            testL_isForest,
                            testL_isSubgraphOf,
                            testL_adj,
                            testL_transpose,
                            testL_union,
                            testL_bft,
                            testL_reachable,
                            testL_isPathOf,
                            testL_path,
                            testL_topo
                            ]

--
-- Teste aleatório
--

--
-- Tarefa 2
--
-- A instância de Arbitrary para grafos definida abaixo gera grafos
-- com muito poucas arestas, como se pode constatar testando a
-- propriedade prop_valid.
-- Defina uma instância de Arbitrary menos enviesada.
-- Este problema ainda é mais grave nos geradores dag e forest que
-- têm como objectivo gerar, respectivamente, grafos que satisfazem
-- os predicados isDag e isForest. Estes geradores serão necessários
-- para testar propriedades sobre estas classes de grafos.
-- Melhore a implementação destes geradores por forma a serem menos enviesados.
--

-- Instância de Arbitrary para arestas
instance Arbitrary v => Arbitrary (Edge v) where
    arbitrary = do s <- arbitrary
                   t <- arbitrary
                   return $ Edge {source = s, target = t}

instance (Ord v, Arbitrary v) => Arbitrary (Graph v) where
    arbitrary = do ns <- arbitrary
                   es <- creatEdges ns
                   return $ Graph {nodes = fromList ns, edges = fromList es}

        where creatEdges ns = frequency [(1, empty), (length (ns) * 3, list ns)]
              empty = return []
              list ns = do a <- elements (ns)
                           b <- elements (ns)
                           l <- creatEdges ns
                           return $ Edge {source = a, target = b} : l
 
prop_valid :: Graph Int -> Property
prop_valid g = collect (length (edges g)) $ isValid g

-- Gerador de DAGs
dag :: (Ord v, Arbitrary v) => Gen (DAG v)
dag = do g <- arbitrary
         return Graph {nodes = nodes g, edges = fromList $ edgesDAG (elems (edges g))}

    where edgesDAG [] = []
          edgesDAG (h@(Edge sr tr):t) = if (sr >= tr)
                                        then edgesDAG (t)
                                        else h : edgesDAG (t)

prop_dag :: Property
prop_dag = forAll (dag :: Gen (DAG Int)) $ \g -> collect (length (edges g)) $ isDAG g

-- Gerador de florestas
forest :: (Ord v, Arbitrary v) => Gen (Forest v)
forest = do g <- dag
            return Graph {nodes = nodes g, edges = fromList $ aux (elems (edges g))}

    where aux [] = []
          aux (h@(Edge sr tr):t) = h : aux (aux2 sr t)

          aux2 _ [] = []
          aux2 sr (h@(Edge o d):t) = if (sr == o)
                                   then aux2 sr t
                                   else h : t

prop_forest :: Property
prop_forest = forAll (forest :: Gen (Forest Int)) $ \g -> collect (length (edges g)) $ isForest g

--
-- Tarefa 3
--
-- Defina propriedades QuickCheck para testar todas as funções
-- do módulo Graph.
--

-- Exemplo de uma propriedade QuickCheck para testar a função adj          
prop_adj :: Graph Int -> Property
prop_adj g = forAll (elements $ elems $ nodes g) $ \v -> adj g v `isSubsetOf` edges g

prop_swap :: Edge Int -> Property
prop_swap (Edge s t) = property $ swap (Edge s t) == (Edge t s)

prop_empty :: Property
prop_empty = property $ Graph.empty == g0

prop_isEmpty :: Graph Int -> Property
prop_isEmpty g = property $ if (isEmpty g)
                            then length (nodes g) == 0
                            else length (nodes g) /= 0

prop_isValid :: Graph Int -> Property
prop_isValid g = property $ isValid g == edgesValid (nodes g) (elems (edges g))
    where edgesValid _ [] = True
          edgesValid n ((Edge sr tr):t) | member sr n && member tr n = edgesValid n t
                                        | otherwise = False

prop_isDAG :: Graph Int -> Property
prop_isDAG g = let b = srcANDtrg (elems (edges g))
                in if (b)
                   then label "Unknown" True
                   else property $ b == isDAG g

    where srcANDtrg [] = True
          srcANDtrg ((Edge sr tr):t) = if (sr == tr)
                                       then False
                                       else srcANDtrg t

prop_isForest :: Property
prop_isForest = forAll (dag :: Gen (DAG Int)) $ aux_isForest

aux_isForest :: DAG Int -> Property
aux_isForest d = property $ isForest (d) == edgesForest (elems (edges d))
    where edgesForest [] = True
          edgesForest ((Edge sr _):t) | exists sr t = False
                                      | otherwise = edgesForest t
          exists _ [] = False
          exists s ((Edge sr _):t) | s == sr = True
                                   | otherwise = exists s t

prop_isSubgraphOf :: Graph Int -> Property
prop_isSubgraphOf g = property $ isSubgraphOf g g == True 

prop_isSubgraphOf1 :: Graph Int -> Property
prop_isSubgraphOf1 g = property $ isSubgraphOf Graph {nodes = (nodes g), edges = (aux)} g == True
    where aux = deleteMax $ deleteMin (edges g)

prop_adj1 :: Graph Int -> Property
prop_adj1 g = (length (nodes g) /= 0)
              ==> forAll (elements $ elems $ nodes g) $ aux_adj1 g

aux_adj1 :: Graph Int -> Int -> Property
aux_adj1 g v = let l = adj g v
                in property $ allSources (elems l)
    where allSources [] = True
          allSources ((Edge sr _):t) = if (sr == v)
                                       then allSources t
                                       else False

prop_transpose :: Graph Int -> Property
prop_transpose g = property $ length (edges g) == length (edges (transpose g))

prop_transpose1 :: Graph Int -> Property
prop_transpose1 g = property $ transpose (transpose (g)) == g

{- Compara cada um dos edges do grafo original com os obtidos
   depois de aplicar a função transpose, verificando se existe 
   o inverso para cada um -}
prop_transpose2 :: Graph Int -> Property
prop_transpose2 g = property $ check_trans (elems $ edges g) (edges $ transpose g) 
    where check_trans [] _ = True
          check_trans ((Edge k w):t) lt = (Edge w k) `member` lt && check_trans t lt 

prop_union :: Graph Int -> Property
prop_union g = property $ Graph.union g g == g

prop_union1 :: Graph Int -> Graph Int -> Property
prop_union1 g1 g2 = property $ length (nodes (Graph.union g1 g2)) <= length (nodes g1) + length (nodes g2)

prop_union2 :: Graph Int -> Graph Int -> Property
prop_union2 g1 g2 = property $ let u = Graph.union g1 g2
                                in (nodes g1) `isSubsetOf` (nodes u) && (nodes g2) `isSubsetOf` (nodes u)
                                    && (edges g1) `isSubsetOf` (edges u) && (edges g2) `isSubsetOf` (edges u)

prop_bft :: Graph Int -> Property
prop_bft g = ((length $ nodes g) /= 0) ==> forAll (elements $ elems $ nodes g) $ aux_bft g 

aux_bft :: Graph Int -> Int -> Property
aux_bft g e = let f = bft g (singleton e)
              in property $ (singleton e) `isSubsetOf` (nodes f)

{- Verifica aux_reachable0 e aux_reachable1-}
prop_reachable :: Graph Int -> Property
prop_reachable g = (length (nodes g) /= 0) 
                   ==> (forAll (elements $ elems $ nodes g) $ aux_reachable0 g) .&&. 
                       (forAll (elements $ elems $ nodes g) $ aux_reachable1 g)

{- Verifica se todos os Nodos devolvidos pela função 
   reachable fazem parte do grafo -}
aux_reachable0 :: Graph Int -> Int -> Property
aux_reachable0 g el = let vs = reachable g el
                      in  property $ aux (elems vs)
    where aux [] = True
          aux (h:t) = (h `member` (nodes g)) && aux t

{- Verifica se todos os Nodos devolvidos pela função
   reachable são target em algum dos edges do grafo -}
aux_reachable1 :: Graph Int -> Int -> Property
aux_reachable1 g el = let vs = reachable g el
                      in property $ aux (elems vs)
    where aux [] = True
          aux (h:t) = isTarget h (elems $ edges g) && aux t
          isTarget x [] = x == el
          isTarget x ((Edge s d):t) = (x == el) || (x == d) || isTarget x t 

prop_isPathOf :: Graph Int -> Property
prop_isPathOf g = (length (edges g) /= 0)
                  ==> forAll (elements $ elems $ edges g) $ \e@(Edge s t) -> isPathOf [e] g

{- Depois de obter um edge aleatorio do grafo, verifica se aplicando
   a função path ao source e target o resultado é diferente de Nothing -} 
prop_path :: Graph Int -> Property
prop_path g = (length (edges g) /= 0) 
              ==> forAll (elements $ elems $ edges g) $ \(Edge x y) -> path g x y /= Nothing

prop_path1 :: Graph Int -> Property
prop_path1 g = (length (edges g) /= 0) ==>
               let els = elems $ edges g
                   src = source $ els!!0
                   trgt = aux src els
                   result = path g src trgt
                in property $ (length $ edges g) >= (length $ fromJust result)

    where aux src [] = src
          aux src ((Edge x y):t) = if (x == src)
                                   then aux y t
                                   else aux src t

prop_topo :: Property
prop_topo = forAll (dag :: Gen (DAG Int)) $ \d -> (length (topo d) <= length (nodes d))

prop_topo1 :: Property
prop_topo1 = forAll (dag :: Gen (DAG Int)) $ aux_topo1

aux_topo1 :: DAG Int -> Property
aux_topo1 d = property $ aux (topo d)
    where aux [] = True
          aux (h:t) | h `isSubsetOf` (nodes d) = aux t
                    | otherwise = False

