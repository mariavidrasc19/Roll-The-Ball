{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
import Data.Array as A

{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East | Null
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}
data Cell = Cell 
    { coord :: Position
    , value :: Char
    } deriving (Eq, Ord)

getValue :: Cell -> Char
getValue (Cell _ valoare) = valoare 

{-
    Tip de date pentru reprezentarea nivelului curent
-}
data Level = Level
    { coords :: Position
    , cellsArray :: (A.Array (Int, Int) Cell)
    }deriving (Eq, Ord)
    
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

{-
    *** TODO ***

    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}
instance Show Level 
    where show (Level size matrix) = "\n" ++ foldr
                                    (\(Cell pos valoare) string -> if (snd pos /= (snd size) - 1)
                                                                    then ([valoare] ++ string)
                                                                    else ([valoare] ++ "\n" ++ string)) "" (A.elems matrix)

{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

makeEmptyCell :: Position -> Cell
makeEmptyCell pos = (Cell pos emptySpace) 

setEmptyLevel :: Position -> A.Array (Int, Int) Cell
setEmptyLevel size = A.array ((0, 0), (fst size, snd size))[((x, y), (makeEmptyCell (x, y))) | x <- [0..(fst size)], y <- [0..(snd size)]]

emptyLevel :: Position -> Level
emptyLevel pos = (Level (fst pos + 1, snd pos + 1) (setEmptyLevel pos))

{-
    *** TODO ***

    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}
elemAt :: (A.Array (Int, Int) Cell) -> (Int, Int) -> Cell
elemAt arr pos = arr A.! pos

addCell :: (Char, Position) -> Level -> Level
addCell (pipeType, pipePosition) (Level size matrix) = if ((fst pipePosition < fst size) && (snd pipePosition < snd size) &&
                                                                                            (fst pipePosition >= 0) && (snd pipePosition >= 0))
                                                                then if ((getValue (elemAt matrix pipePosition)) == emptySpace) 
                                                                        then (Level size (matrix A.// [(pipePosition, (Cell pipePosition pipeType))]))
                                                                        else (Level size matrix)
                                                                else (Level size matrix)


{-
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}
 
createLevel :: Position -> [(Char, Position)] -> Level
createLevel maxPos list = (Level (fst maxPos + 1, snd maxPos + 1) (foldr (\(info, pos) acc -> acc A.// [(pos, (Cell pos info))]) (setEmptyLevel maxPos) list))


{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

interschimbareElem :: Level -> Cell -> Cell -> Char -> Level
interschimbareElem (Level size matrix) (Cell pos1 _) (Cell pos2 _) info = (createLevel (fst size - 1, snd size - 1)
                                                                                        (foldr (\(Cell pos val) acc -> if (fst pos == fst pos1) && (snd pos == snd pos1)
                                                                                                                then (emptySpace, pos) : acc
                                                                                                                else if (fst pos == fst pos2) && (snd pos == snd pos2)
                                                                                                                        then (info, pos) : acc
                                                                                                                        else (val, pos) : acc) [] (A.elems matrix)))

esteMutabila :: Cell -> Bool
esteMutabila (Cell _ val) = if (val /= startRight) && (val /= startLeft) && (val /= startDown) && (val /= startUp) && 
                                (val /= winRight) && (val /= winLeft) && (val /= winDown) && (val /= winUp) then True else False

moveCell :: Position -> Directions -> Level -> Level
moveCell pos dirPiece (Level size matrix)
    | dirPiece == North && (fst pos - 1 >= 0) && getValue (elemAt matrix (fst pos - 1, snd pos)) == emptySpace && esteMutabila (elemAt matrix pos) == True = 
            interschimbareElem (Level size matrix) (elemAt matrix pos) (elemAt matrix (fst pos - 1, snd pos)) (getValue (elemAt matrix pos))
    | dirPiece == South && (fst pos + 1 < fst size) && getValue (elemAt matrix (fst pos + 1, snd pos)) == emptySpace && esteMutabila (elemAt matrix pos) == True = 
            interschimbareElem (Level size matrix) (elemAt matrix pos) (elemAt matrix (fst pos + 1, snd pos)) (getValue (elemAt matrix pos))
    | dirPiece == West && (snd pos - 1 >= 0) && getValue (elemAt matrix (fst pos, snd pos - 1)) == emptySpace && esteMutabila (elemAt matrix pos) == True = 
            interschimbareElem (Level size matrix) (elemAt matrix pos) (elemAt matrix (fst pos, snd pos - 1)) (getValue (elemAt matrix pos))
    | dirPiece == East && (snd pos + 1 < snd size) && getValue (elemAt matrix (fst pos, snd pos + 1)) == emptySpace && esteMutabila (elemAt matrix pos) == True = 
            interschimbareElem (Level size matrix) (elemAt matrix pos) (elemAt matrix (fst pos, snd pos + 1)) (getValue (elemAt matrix pos))
    | otherwise = (Level size matrix)

{-
    *** HELPER ***

    Verifică dacă două celule se pot conecta.
    Atenție: Direcția indică ce vecin este a
    doua celulă pentru prima.

    ex: connection botLeft horPipe East = True (╚═)
        connection horPipe botLeft East = False (═╚)
-}
connection :: Cell -> Cell -> Directions -> Bool
connection (Cell _ val1) (Cell _ val2) directionOf2
    | val1 == verPipe && val2 == verPipe && directionOf2 == North = True
    | val1 == verPipe && val2 == topRight && directionOf2 == North = True
    | val1 == verPipe && val2 == topLeft && directionOf2 == North = True 
    | val1 == verPipe && val2 == winDown && directionOf2 == North = True
    | val1 == verPipe && val2 == verPipe && directionOf2 == South = True
    | val1 == verPipe && val2 == botRight && directionOf2 == South = True
    | val1 == verPipe && val2 == botLeft && directionOf2 == South = True 
    | val1 == verPipe && val2 == winUp && directionOf2 == South = True 
    | val1 == horPipe && val2 == horPipe && directionOf2 == East = True
    | val1 == horPipe && val2 == botRight && directionOf2 == East = True
    | val1 == horPipe && val2 == topRight && directionOf2 == East = True
    | val1 == horPipe && val2 == winLeft && directionOf2 == East = True
    | val1 == horPipe && val2 == horPipe && directionOf2 == West = True
    | val1 == horPipe && val2 == botLeft && directionOf2 == West = True
    | val1 == horPipe && val2 == topLeft && directionOf2 == West = True
    | val1 == horPipe && val2 == winRight && directionOf2 == West = True
    | val1 == botLeft && val2 == horPipe && directionOf2 == East = True
    | val1 == botLeft && val2 == botRight && directionOf2 == East = True
    | val1 == botLeft && val2 == topRight && directionOf2 == East = True
    | val1 == botLeft && val2 == winLeft && directionOf2 == East = True
    | val1 == botLeft && val2 == verPipe && directionOf2 == North = True
    | val1 == botLeft && val2 == topRight && directionOf2 == North = True
    | val1 == botLeft && val2 == topLeft && directionOf2 == North = True
    | val1 == botLeft && val2 == winDown && directionOf2 == North = True
    | val1 == topLeft && val2 == horPipe && directionOf2 == East = True
    | val1 == topLeft && val2 == botRight && directionOf2 == East = True
    | val1 == topLeft && val2 == topRight && directionOf2 == East = True
    | val1 == topLeft && val2 == winLeft && directionOf2 == East = True
    | val1 == topLeft && val2 == verPipe && directionOf2 == South = True
    | val1 == topLeft && val2 == botRight && directionOf2 == South = True
    | val1 == topLeft && val2 == botLeft && directionOf2 == South = True 
    | val1 == topLeft && val2 == winUp && directionOf2 == South = True 
    | val1 == botRight && val2 == horPipe && directionOf2 == West = True
    | val1 == botRight && val2 == botLeft && directionOf2 == West = True
    | val1 == botRight && val2 == topLeft && directionOf2 == West = True
    | val1 == botRight && val2 == winRight && directionOf2 == West = True
    | val1 == botRight && val2 == verPipe && directionOf2 == North = True
    | val1 == botRight && val2 == topRight && directionOf2 == North = True
    | val1 == botRight && val2 == topLeft && directionOf2 == North = True
    | val1 == botRight && val2 == winDown && directionOf2 == North = True
    | val1 == topRight && val2 == horPipe && directionOf2 == West = True
    | val1 == topRight && val2 == botLeft && directionOf2 == West = True
    | val1 == topRight && val2 == topLeft && directionOf2 == West = True
    | val1 == topRight && val2 == winRight && directionOf2 == West = True
    | val1 == topRight && val2 == verPipe && directionOf2 == South = True
    | val1 == topRight && val2 == botRight && directionOf2 == South = True
    | val1 == topRight && val2 == botLeft && directionOf2 == South = True
    | val1 == topRight && val2 == winUp && directionOf2 == South = True
    | val1 == startUp && val2 == verPipe && directionOf2 == North = True
    | val1 == startUp && val2 == topRight && directionOf2 == North = True
    | val1 == startUp && val2 == topLeft && directionOf2 == North = True
    | val1 == startUp && val2 == winDown && directionOf2 == North = True
    | val1 == startDown && val2 == verPipe && directionOf2 == South = True
    | val1 == startDown && val2 == botRight && directionOf2 == South = True
    | val1 == startDown && val2 == botLeft && directionOf2 == South = True
    | val1 == startDown && val2 == winUp && directionOf2 == South = True
    | val1 == startLeft && val2 == horPipe && directionOf2 == West = True
    | val1 == startLeft && val2 == botLeft && directionOf2 == West = True
    | val1 == startLeft && val2 == topLeft && directionOf2 == West = True
    | val1 == startLeft && val2 == winRight && directionOf2 == West = True
    | val1 == startRight && val2 == horPipe && directionOf2 == East = True
    | val1 == startRight && val2 == botRight && directionOf2 == East = True
    | val1 == startRight && val2 == topRight && directionOf2 == East = True
    | val1 == startRight && val2 == winLeft && directionOf2 == East = True
    | otherwise = False

{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}

verificarePath :: Level -> Cell -> Directions -> Bool
verificarePath (Level size matrix) (Cell pos val) pastDir
    | val `elem` [winDown, winLeft, winRight, winUp] = True
    | (pastDir /= North) && (fst pos - 1 >= 0) && (connection (Cell pos val) (elemAt matrix (fst pos - 1, snd pos)) North) == True 
            = verificarePath (Level size matrix) (elemAt matrix (fst pos - 1, snd pos)) South
    | (pastDir /= South) && (fst pos + 1 < fst size) && (connection (Cell pos val) (elemAt matrix (fst pos + 1, snd pos)) South) == True 
            = verificarePath (Level size matrix) (elemAt matrix (fst pos + 1, snd pos)) North
    | (pastDir /= East) && (snd pos + 1 < snd size) && (connection (Cell pos val) (elemAt matrix (fst pos, snd pos + 1)) East) == True 
            = verificarePath (Level size matrix) (elemAt matrix (fst pos, snd pos + 1)) West
    | (pastDir /= West) && (snd pos - 1 >= 0) && (connection (Cell pos val) (elemAt matrix (fst pos, snd pos - 1)) West) == True 
            = verificarePath (Level size matrix) (elemAt matrix (fst pos, snd pos - 1)) East
    | otherwise = False

wonLevel :: Level -> Bool
wonLevel (Level size matrix) = verificarePath (Level size matrix) (head (filter (\(Cell _ val) -> 
                        if val `elem` [startRight, startLeft, startDown, startUp]
                            then True 
                            else False) (A.elems matrix))) Null

mutariPosibile :: Level -> Cell -> Directions -> [((Position, Directions), Level)]
mutariPosibile (Level size matrix) (Cell pos _) dir
    | (dir == North) && (fst pos - 1 >= 0) && (getValue (elemAt matrix (fst pos - 1, snd pos))) == emptySpace = [((pos, North), (moveCell pos North (Level size matrix)))]
    | (dir == South) && (fst pos + 1 < fst size) && (getValue (elemAt matrix (fst pos + 1, snd pos))) == emptySpace = [((pos, South), (moveCell pos South (Level size matrix)))]
    | (dir == West) && (snd pos - 1 >= 0) && (getValue (elemAt matrix (fst pos, snd pos - 1))) == emptySpace = [((pos, West), (moveCell pos West (Level size matrix)))]
    | (dir == East) && (snd pos + 1 < snd size) && (getValue (elemAt matrix (fst pos, snd pos + 1))) == emptySpace = [((pos, East), (moveCell pos East (Level size matrix)))]
    | otherwise = []


instance ProblemState Level (Position, Directions) where
    successors (Level size matrix) = foldl (\acc (Cell pos val) -> if (val /= emptySpace) && (esteMutabila (Cell pos val)) 
                                                                    then acc ++ mutariPosibile (Level size matrix) (Cell pos val) North
                                                                                ++ mutariPosibile (Level size matrix) (Cell pos val) South
                                                                                    ++ mutariPosibile (Level size matrix) (Cell pos val) East
                                                                                        ++ mutariPosibile (Level size matrix) (Cell pos val) West
                                                                    else acc) [] (A.elems matrix)

    isGoal = wonLevel

    reverseAction ((pos, dir), (Level size matrix)) = if (dir == North) then (((fst pos + 1, snd pos), South), (interschimbareElem (Level size matrix) (elemAt matrix pos) (elemAt matrix (fst pos + 1, snd pos)) (getValue (elemAt matrix pos))))
                                                                         else if (dir == South) then (((fst pos - 1, snd pos), North), (interschimbareElem (Level size matrix) (elemAt matrix pos) (elemAt matrix (fst pos - 1, snd pos)) (getValue (elemAt matrix pos))))
                                                                                                else if (dir == East) then (((fst pos, snd pos + 1), West), (interschimbareElem (Level size matrix) (elemAt matrix pos) (elemAt matrix (fst pos, snd pos + 1)) (getValue (elemAt matrix pos))))
                                                                                                                      else if (dir == West) then (((fst pos, snd pos - 1), East), (interschimbareElem (Level size matrix) (elemAt matrix pos) (elemAt matrix (fst pos, snd pos - 1)) (getValue (elemAt matrix pos))))
                                                                                                                                            else ((pos, dir), (Level size matrix))
