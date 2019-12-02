-- combine the two fns
mapValue = sub . thirdFloor

-- first, divide by three; then we'd need to floor 
-- as we'd need to round it down, we can just get the quotient. 
-- it also looks fancier :-)
thirdFloor a = a `quot` 3

-- dizz just cause I want to use tacit style lol
sub b = b - 2
 
checkResult = foldr (+) 0 . map mapValue
