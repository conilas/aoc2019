fullRange start end = [start .. end]

-- utils

toDigitRec number acc =
  let current = number `div` 10 in
    if current == 0 then (number, acc) else toDigitRec current (acc * 10)

toDigit number =
  toDigitRec number 1

decRepresentationToNumber value =
  fst value * snd value

collectDigitsRec number acc =
  let newDigit = toDigit number in
    let newNumber = number - (decRepresentationToNumber newDigit) in
      let newAcc = acc ++ [fst newDigit] in
        if newNumber == 0 then newAcc else collectDigitsRec newNumber newAcc

collectDigits number = collectDigitsRec number []

-- our predicates

-- check if ordered
isOrdered [x] = True
isOrdered (x:xs) = if x > head xs then False else isOrdered xs

-- map to pairs, check if we have the necessary pair
toPairsRec [x] acc = acc
toPairsRec (x:xs) acc = toPairsRec xs (acc ++ [(x, head xs)])
toPairs list = toPairsRec list []

matchEqualPair [] = False
matchEqualPair (x:xs) =
  if (fst x == snd x) then True else matchEqualPair xs

hasPair = matchEqualPair . toPairs

toListOfPairsRec [] accList acc = accList ++ [acc]
toListOfPairsRec (x:xs) accList (y:ys) =
  let acc = (y:ys) in
    if (x == head acc) then toListOfPairsRec xs accList (acc ++ [x]) else toListOfPairsRec xs (accList ++ [acc]) [x]
toListOfPairsRec (x:xs) accList [] =
  if (x == head xs) then toListOfPairsRec (tail xs) accList ([x, head xs]) else toListOfPairsRec xs [] []
toListOfPairs list =
  toListOfPairsRec list [] []

twoPair [] = False
twoPair (x:xs) =
  if length x == 2 then True else twoPair xs

-- actually callable functions

hasSixDigits = ((==) 6) . length

filterCorrectList list = [x | x <- list, hasSixDigits x, isOrdered x, hasPair x]

mkList begin = filterCorrectList . (map collectDigits) . (fullRange begin)

-- needed othewise WSL will explode your memory
-- so compile then run
main = do
  print (length (mkList 402328 864247 )) -- first part
  print (length (filter twoPair (map toListOfPairs (mkList 402328 864247)))) -- second part
