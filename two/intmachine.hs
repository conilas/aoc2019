import Data.Sort

-- utils
fstT (a,_,_) = a
sndT (_,a,_) = a
thrT (_,_,a) = a

--we need a main fn with a list which will contain
fromListRetrieve replacementList neededPosition = filter (isValidRepresentation neededPosition) replacementList 

isValidRepresentation position testedValue = position == fstT testedValue

transformToState values = zipWith (\x y -> (y, x, 0)) values [0..]

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

-- argument invert cause I'm lazy
fromListRetrieveLastInv neededPosition replacementList = fromListRetrieveLast replacementList neededPosition

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

-- transform the opcode to the operation
opcode (Just 1)  = Just (+)
opcode (Just 2)  = Just (*)
opcode (Just 99) = Nothing
opcode (Just _) = Nothing
opcode (Nothing) = Nothing

buildNewListOnRec rewriteIndex currentList Nothing = currentList
buildNewListOnRec rewriteIndex currentList (Just value) = buildNewList currentList (rewriteIndex, value)

-- mutual rec function so we can actually end it
operateRecurse Nothing _ _ list _ _ = list
operateRecurse op left right list index (Just rewriteIndex) = let v = op <*> (fmap sndT left) <*> (fmap sndT right) in 
                                                         findValueRec (index + 4) (buildNewListOnRec rewriteIndex list v)

-- recurse on list untill we die
findValueRec lookupIndex values = let retrievedValue = fromListRetrieveLast values lookupIndex in 
                                    let selectedCode = opcode (fmap sndT retrievedValue) in
                                      let leftIndex = fmap sndT (fromListRetrieveLast values (lookupIndex + 1)) in
                                        let rightIndex = fmap sndT (fromListRetrieveLast values (lookupIndex + 2)) in
                                          let newIndex = fmap sndT (fromListRetrieveLast values (lookupIndex + 3)) in
                                            let (rightValue, leftValue) = (rightIndex >>= (fromListRetrieveLast values), leftIndex >>= (fromListRetrieveLast values)) in
                                              operateRecurse selectedCode rightValue leftValue values lookupIndex newIndex

findValueRecZero = findValueRec 0

-- call rec starting with initial state and empty rewrite rules
applyStateMachine = findValueRecZero . transformToState

getLastNthArg arg = fmap sndT . fromListRetrieveLastInv arg . applyStateMachine 
