import Data.Sort

-- utils
fstT (a,_,_) = a
sndT (_,a,_) = a
thrT (_,_,a) = a

--we need a main fn with a list which will contain
fromListRetrieve replacementList neededPosition = filter (isValidRepresentation neededPosition) replacementList 

isValidRepresentation position testedValue = position == fstT testedValue

-- make index for our list of triples checking whether 
-- we should put the first one there or add a new one
-- TODO make the next operation equal so we can chain them better. they are almost dasame goddamit
makeIndex :: [(Int, Int, Int)] -> Int
makeIndex [] = 0
makeIndex (x:xs) = thrT x + 1

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

-- usefull api (functions to call to represent state)

-- retrieves the current value or empty
fromListRetrieveLast replacementList neededPosition = let filteredList = fromListRetrieve replacementList neededPosition in 
                                                        let sorted = sortOn thrT filteredList in
                                                          safeHead (reverse sorted)

-- increments the list in the new position
buildNewList :: [(Int, Int, Int)] -> (Int, Int) -> [(Int, Int, Int)]
buildNewList currentList positionValue = let filteredList = fromListRetrieve currentList (fst positionValue) in
                                           let sorted = sortOn thrT filteredList in
                                             let nextIndex = makeIndex (reverse sorted) in
                                               let newTriple = [(fst positionValue, snd positionValue, nextIndex)] in
                                                 currentList ++ newTriple


-- recurse on list untill we die
-- findValueRec [] state = state
-- findValueRec (x:xs) state = 

-- call rec starting with initial state and empty rewrite rules
findValue initialState = findValueRec initialState []

