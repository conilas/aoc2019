import Data.Sort

-- util
subtractT :: (Int, Int) -> (Int, Int) -> (Int, Int)
subtractT (a,b) (x,y) = (x-a, y-b)

sumT :: (Int, Int) -> Int
sumT (a,b) = abs a + abs b

intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect (x:xs) ys
    | x `elem` ys = x : intersect xs ys
    | otherwise = intersect xs ys

findInListRec (x:xs) idx element =
  if element == x then idx else findInListRec xs (idx + 1) element

findInList list element = findInListRec list 0 element

-- datatype to make it easy to command our little girl going through
data Command = U Int | D Int | L Int | R Int
  deriving (Show)

-- new pos calculator. could be up | down | left | right
-- buuut I wanted to make it easier when inputting stuff
newPosition (x,y) (U a) = (x, y+a)
newPosition (x,y) (D a) = (x, y-a)
newPosition (x,y) (L a) = (x-a, y)
newPosition (x,y) (R a) = (x+a, y)

-- kindof a state reducer
unzipNewPositions lastPosition (U a) =
  let constructions = map U [z | z <- [a, (a-1)..1]] in
    sortOn snd (map (newPosition lastPosition) constructions)

unzipNewPositions lastPosition (D a) =
  let constructions = map D [z | z <- [a, (a-1)..1]] in
    reverse (sortOn snd (map (newPosition lastPosition) constructions))

unzipNewPositions lastPosition (L a) =
  let constructions = map L [z | z <- [a, (a-1)..1]] in
    reverse (sortOn fst (map (newPosition lastPosition) constructions))

unzipNewPositions lastPosition (R a) =
  let constructions = map R [z | z <- [a, (a-1)..1]] in
    sortOn fst (map (newPosition lastPosition) constructions)

walk currentPositions command =
  let lastPosition = last currentPositions in
    currentPositions ++ (unzipNewPositions lastPosition command)

walkAll currentState (x:xs) = walkAll (walk currentState x) xs
walkAll currentState []     = currentState


-- call ex: findAllIntersectionFromCommands [R 98,U 47,R 26,D 63,R 33,U 87,L 62,D 20,R 33,U 53,R 51] [U 98,R 91,D 20,R 16,D 67,R 40,U 7,R 15,U 6,R 7]
-- output ex: [(1,1),(108,48),(125,12),(158,19),(108,72),(108,52)]
-- in order to test, call findAllIntersectionFromCommands, fetch the list and
-- call findLeastDifferenceInListFromPoint to find the least difference built from the first
-- command
findMoveMatrix first second =
  let firstMatrix = walkAll [(1,1)] first in
    let secondMatrix = walkAll [(1,1)] second in
      (firstMatrix, secondMatrix)

findAllIntersectionFromCommands :: [Command] -> [Command] -> [(Int, Int)]
findAllIntersectionFromCommands first second =
  let matrices = findMoveMatrix first second in
    intersect (fst matrices) (snd matrices)

findLeastDifferenceInListFromPoint :: (Int, Int) -> [(Int, Int)] -> Int
findLeastDifferenceInListFromPoint startingPoint list =
  head (sort (tail (map sumT (map (subtractT startingPoint) list))))

findSpotClosestToCentralStation :: [(Int, Int)] -> Int
findSpotClosestToCentralStation = findLeastDifferenceInListFromPoint (1,1)

-- part one - find the closest possible spot
findClosestSpot a b = findSpotClosestToCentralStation (findAllIntersectionFromCommands a b)

-- part two - find costs (we could have taken tail but meh, just look at the IO stream)
findCostForIntersection :: [(Int, Int)] -> [(Int, Int)] -> (Int, Int) -> Int
findCostForIntersection first second intersected =
  let firstCost = findInList first intersected in
    let secondCost = findInList second intersected in
      firstCost + secondCost

findMoveMatrixWithCosts :: [Command] -> [Command] -> [Int]
findMoveMatrixWithCosts first second =
  let matrices = findMoveMatrix first second in
    let firstMatrix = fst matrices in
      let secondMatrix = snd matrices in
        let intersection = intersect firstMatrix secondMatrix in
          map (findCostForIntersection firstMatrix secondMatrix) intersection
