{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import Data.Maybe
--import RollTheBall
--import Pipes
import ProblemState
{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate

level1List :: [(Char, Position)]
level1List = [
    (startDown, (0,0)), (emptyCell, (0,1)), (emptyCell, (0,2)), (emptyCell, (0,3)),
    (verPipe, (1,0)), (emptyCell, (1,1)), (emptyCell, (1,2)), (emptyCell, (1,3)),
    (verPipe, (2,0)), (horPipe, (2,1)), (emptyCell, (2,2)), (emptyCell, (2,3)),
    (botLeft, (3,0)), (horPipe, (3,2)), (winLeft, (3,3))
    ]

level1String :: [Char]
level1String = [ endl,
    startDown, emptyCell, emptyCell, emptyCell, endl,
    verPipe, emptyCell, emptyCell, emptyCell, endl,
    verPipe, horPipe, emptyCell, emptyCell, endl,
    botLeft, emptySpace, horPipe, winLeft, endl
    ]

level1 :: Level
level1 = createLevel (3,3) level1List
-}

data Node s a = Node 
    { stare :: s
    , actiune :: Maybe a
    , copii :: [Node s a]
    , parinte :: Maybe (Node s a)
    , adancime :: Int
    } deriving(Eq, Show, Ord)

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}
nodeState :: Node s a -> s
nodeState (Node state _ _ _ _) = state

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent (Node _ _ _ parent _) = parent

nodeDepth :: Node s a -> Int
nodeDepth (Node _ _ _ _ d) = d

nodeAction :: Node s a -> Maybe a
nodeAction (Node _ action _ _ _) = action

nodeChildren :: Node s a -> [Node s a]
nodeChildren (Node _ _ children _ _) = children

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}
function :: (ProblemState s a, Eq s) => Node s a -> [Node s a]
function (Node state action children parent h) = map (\(act, st) -> generateChild st act (Node state action children parent h)) (successors state)

generateChild :: (ProblemState s a, Eq s) => s -> a -> Node s a -> Node s a
generateChild stateChild actionChild (Node stateParent actionParent children parent h) = nod
    where
        nod = (Node stateChild (Just actionChild) (function nod) (Just (Node stateParent actionParent children parent h)) (1 + h)) 

generateRoot :: (ProblemState s a, Eq s) => s -> Node s a
generateRoot state = root 
    where
        root = (Node state Nothing (function root) Nothing 1)

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace state = generateRoot state 

{-
    *** TODO ***
   
    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera

-}
lastEl :: (Eq a, Eq s, Ord s) => Node s a -> [Node s a] -> [Node s a] -> [([Node s a], [Node s a])]
lastEl nod coada visited = if null (filter (\nod1 -> if (nodeState nod) == (nodeState nod1) then True else False) visited) then [(nodeChildren nod, ((tail coada) ++ (nodeChildren nod)))] else []

isVisited :: (Eq a, Eq s, Ord s) => Node s a -> [Node s a] -> Bool
isVisited nod visited = if (filter (\nod1 -> if (nod1 == nod) then True else False) visited) == [] then False else True

generateBfsList :: (Eq a, Eq s, Ord s) => [([Node s a], [Node s a])] -> [Node s a] -> [([Node s a], [Node s a])]
generateBfsList list visited = if isVisited (head . snd . last $ list) visited
            then list ++ generateBfsList (lastEl (head . tail . snd . last $ list) (tail . snd . last $ list) visited) visited
            else list ++ generateBfsList (lastEl (head . snd . last $ list) (snd . last $ list) visited) (visited ++ [head . snd . last $ list])

bfs :: (Eq a, Eq s, Ord s) => Node s a -> [([Node s a], [Node s a])]
bfs node = generateBfsList [([node], [node])] []


{-
    *** TODO ***
  
    Primește starea inițială și finală și întoarce o pereche de noduri, reprezentând
    intersecția dintre cele două frontiere.
-}
stateInList :: (Eq a, Eq s, Ord s) => Node s a -> [Node s a] -> [(Node s a, Node s a)]
stateInList nod lista = if (filter (\nod1 -> if (nodeState nod) == (nodeState nod1) then True else False) lista) == [] then [] else [(nod, head (filter (\nod1 -> if (nodeState nod) == (nodeState nod1) then True else False) lista))]

verificareFrontiere :: (Eq a, Eq s, Ord s) => [([Node s a], [Node s a])] -> [([Node s a], [Node s a])] -> (Node s a, Node s a)
verificareFrontiere list1 list2 = if filtrare == []
                                    then verificareFrontiere (tail list1) (tail list2)
                                        else head filtrare
    where
        filtrare = foldl (\acc nod -> if (stateInList nod (fst . head $ list1)) == [] then acc else acc ++ (stateInList nod (fst . head $ list1))) [] (snd . head $ list2)

bidirBFS :: (Eq a, Eq s, Ord s) => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS nod1 nod2 = verificareFrontiere (bfs $ nod1) (bfs $ nod2)


{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.

-}


extractPath :: (Eq a, Eq s) => Node s a -> [(Maybe a, s)]
extractPath nod = if (nodeParent nod) /= Nothing then extractPath (fromJust (nodeParent nod)) ++ [(nodeAction nod, nodeState nod)]
                                                                            else [(nodeAction nod, nodeState nod)]


{-
    *** TODO ***

    Pornind de la o stare inițială și una finală, se folosește de bidirBFS pentru a găsi
    intersecția dintre cele două frontiere și de extractPath pentru a genera calea.

    Atenție: Pentru calea gasită în a doua parcurgere, trebuie să aveți grijă la a asocia
    corect fiecare stare cu acțiunea care a generat-o.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}

solve :: (ProblemState s a, Ord s, Eq a, Eq s)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilo
solve = undefined
      {-
prelucrareLista :: [(Maybe a, s)] -> [(a, s)]
prelucrareLista lista = map (\(act, st) -> (fromJust act, st)) lista

extractPath2 :: [(Maybe a, s)] -> [(Maybe a, s)]
extractPath2 list = zip (foldl (\acc (Just act, st) -> acc ++ [Just act]) [] list) (tail (foldl (\acc (Just act, st) -> acc ++ [st]) [] list))

solve stateInitial stateFinal = (extractPath (fst nodes))
                                            ++ (prelucrareLista . extractPath2 . reverse . extractPath $ (snd nodes))
    where
        nodes = bidirBFS (createStateSpace stateInitial) (createStateSpace stateFinal)
        -}