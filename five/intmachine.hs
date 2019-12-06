import Data.Sort

-- utils
fstT (a,_,_) = a
sndT (_,a,_) = a
thrT (_,_,a) = a

--we need a main fn with a list which will contain
fromListRetrieve replacementList neededPosition =
  filter (isValidRepresentation neededPosition) replacementList

isValidRepresentation position testedValue =
  position == fstT testedValue

transformToState values =
  zipWith (\x y -> (y, x, 0)) values [0..]

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
fromListRetrieveLastInv neededPosition replacementList =
  fromListRetrieveLast replacementList neededPosition

-- retrieves the current value or empty
fromListRetrieveLast replacementList neededPosition =
  let filteredList = fromListRetrieve replacementList neededPosition in
    let sorted = sortOn thrT filteredList in
      safeHead (reverse sorted)

-- increments the list in the new position
buildNewList :: [(Int, Int, Int)] -> (Int, Int) -> [(Int, Int, Int)]
buildNewList currentList positionValue =
  let filteredList = fromListRetrieve currentList (fst positionValue) in
    let sorted = sortOn thrT filteredList in
      let nextIndex = makeIndex (reverse sorted) in
        let newTriple = [(fst positionValue, snd positionValue, nextIndex)] in
          currentList ++ newTriple

data State = Continue | Stopped | IO

type OpCodeOperation = [(Int, Int, Int)] -> Int -> ([(Int,Int,Int)], Int, State)

opCodeArithmeticOperationTwoArity values lookupIndex op = 
  let leftIndex = fmap sndT (fromListRetrieveLast values (lookupIndex + 1)) in
    let rightIndex = fmap sndT (fromListRetrieveLast values (lookupIndex + 2)) in
      let newIndex = fmap sndT (fromListRetrieveLast values (lookupIndex + 3)) in
        let (right, left) = (rightIndex >>= (fromListRetrieveLast values), leftIndex >>= (fromListRetrieveLast values)) in
          let newValue = Just op <*> (fmap sndT left) <*> (fmap sndT right) in
            let newList = buildNewListOnRec newIndex values newValue in
              (newList, lookupIndex + 4, Continue)

opCodeAdd values lookupIndex = opCodeArithmeticOperationTwoArity values lookupIndex (+)
opCodeTimes values lookupIndex = opCodeArithmeticOperationTwoArity values lookupIndex (*)
opCodeInput list currentIndex = (list, currentIndex, IO)-- get first three from list, operate, return list
opCodeOutput list currentIndex = (list, currentIndex, Continue)-- get first three from list, operate, return list

-- transform the opcode to the operation
opcode :: Maybe Int -> Maybe OpCodeOperation
opcode (Just 1)  = Just opCodeAdd
opcode (Just 2)  = Just opCodeTimes
opcode (Just 3)  = Just opCodeInput
opcode (Just 4)  = Just opCodeOutput
opcode (Just 99) = Nothing
opcode (Just _)  = Nothing
opcode (Nothing) = Nothing

buildNewListOnRec _ currentList Nothing = currentList
buildNewListOnRec (Just rewriteIndex) currentList (Just value) = buildNewList currentList (rewriteIndex, value)

-- mutual rec function so we can actually end it

-- recurse on list untill we die

-- This handler case is to handle IO from the main fn
findValueRec lookupIndex values (Just input) =
  let toInputIntoIndex = fmap sndT (fromListRetrieveLast values (lookupIndex + 1)) in
    let newList = buildNewListOnRec toInputIntoIndex values input in
      findValueRec (lookupIndex + 2) newList Nothing

findValueRec lookupIndex values Nothing =
  let retrievedValue = fromListRetrieveLast values lookupIndex in
    let selectedCode = opcode (fmap sndT retrievedValue) in
      case selectedCode of Nothing -> (values, lookupIndex, Stopped)
                           Just op -> let valuesFromOp = op values lookupIndex in -- this case will handle the state of the state machine
                                        case valuesFromOp of (values, lookupIndex, IO) -> (values, lookupIndex, IO)
                                                             (values, lookupIndex, _)  -> findValueRec (sndT valuesFromOp) (fstT valuesFromOp) Nothing

findValueRecZero values = findValueRec 0 values Nothing

-- call rec starting with initial state and empty rewrite rules
applyStateMachine fin sin =
  findValueRecZero . (applyInput fin sin) . transformToState
applyInput fin sin init =
  let firstList = buildNewList init (1, fin) in
    buildNewList firstList (2, sin)

-- TODO: correct the brute-force-thingy
getLastNthArg arg fin sin init = Just 0 

getOutput fin sin =
  getLastNthArg 0 fin sin

-- fuzzy untill output (worst case is ... a lot of time lol)
nextPair fin sin =
  if sin == 99 then (fin + 1, 0) else (fin, sin + 1)

testRecMut curFin curSin desired init =
  let nextValues = nextPair curFin curSin in
    testRec (fst nextValues) (snd nextValues) desired init

checkEndCondition :: Maybe Int -> Int -> Bool
checkEndCondition Nothing _ = False
checkEndCondition (Just a) b = a == b

testRec fin sin desired init =
  if checkEndCondition (getOutput fin sin init) desired
  then (fin, sin) else testRecMut fin sin desired init

testUntill desired init =
  testRec 0 0 desired init


main = do
  x <- return (findValueRecZero (transformToState [1,9,10,3,2,3,11,0,99,30,40,50]))
  case x of (list, index, Stopped) -> print list
            (list, index, IO) -> print [(0,0,0)]
            (list, index, Continue) -> print [(999,999,999)] 

