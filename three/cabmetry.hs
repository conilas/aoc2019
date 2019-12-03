import Data.Sort

-- util
reduce :: (Int, Int) -> (Int, Int) -> (Int, Int)
reduce (a,b) (x,y) = (x-a, y-b)

sumT :: (Int, Int) -> Int
sumT (a,b) = abs a + abs b

intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect (x:xs) ys
    | x `elem` ys = x : intersect xs ys
    | otherwise = intersect xs ys

-- datatype to make it easy to command our little girl going through
data Command = U Int | D Int | L Int | R Int
  deriving (Show)

-- new pos calculator
newPosition (x,y) (U a) = (x, y+a)
newPosition (x,y) (D a) = (x, y-a)
newPosition (x,y) (L a) = (x-a, y)
newPosition (x,y) (R a) = (x+a, y)

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

-- in order to test, call findAllIntersectionFromCommands, fetch the list and
-- call findLeastDifferenceInList to find the least difference built from the first
-- command

-- call ex: findAllIntersectionFromCommands [R 98,U 47,R 26,D 63,R 33,U 87,L 62,D 20,R 33,U 53,R 51] [U 98,R 91,D 20,R 16,D 67,R 40,U 7,R 15,U 6,R 7]
-- output ex: [(1,1),(108,48),(125,12),(158,19),(108,72),(108,52)]
findAllIntersectionFromCommands :: [Command] -> [Command] -> [(Int, Int)]
findAllIntersectionFromCommands first second =
  let firstPlaces = walkAll [(1,1)] first in
    let secondPlaces = walkAll [(1,1)] second in
      intersect firstPlaces secondPlaces

findLeastDifferenceInList :: (Int, Int) -> [(Int, Int)] -> Int
findLeastDifferenceInList startingPoint list =
  head (sort (tail (map sumT (map (reduce startingPoint) list))))

findLeastDifferenceChallenge :: [(Int, Int)] -> Int
findLeastDifferenceChallenge a = findLeastDifferenceInList (1,1) a

-- completeChallenge :: [Command] -> [Command] -> Int
completeChallenge a b = findLeastDifferenceChallenge (findAllIntersectionFromCommands a b)
