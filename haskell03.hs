
add10toall :: [Int] -> [Int]
add10toall l = [x + 10 | x <- l]

multN :: Int -> [Int] -> [Int]
multN n l = [x * n | x <- l]

multN' :: Int -> [Int] -> [Int]
multN' n l = map (\ l -> l * n) l

applyExpr :: [Int] -> [Int]
applyExpr l = [3*x+2 | x <- l]

applyExpr' :: [Int] -> [Int]
applyExpr' l = map (\ l -> 3*l+2) l

addSuffix :: String -> [String] -> [String]
addSuffix str strl = [x ++ str | x <- strl]

selectgt5 :: [Int] -> [Int]
selectgt5 l = [x | x <- l, x > 5]

sumOdds :: [Int] -> Int
sumOdds l = sum [ x | x <- l, odd x]

sumOdds' :: [Int] -> Int
sumOdds' l = sum (filter odd l)

selectExpr :: [Int] -> [Int]
selectExpr l = [x | x <- l, even x, x > 20, x < 50]

countShorts :: [String] -> Int
countShorts strl = length [str | str <- strl, length str < 5]

calcExpr :: [Float] -> [Float]
calcExpr l = [x^2/2 | x <- l, x^2/2 > 10]

trSpaces :: String -> String
trSpaces str = [if c == ' ' then '-' else c | c <- str]

selectSnd :: [(Int,Int)] -> [Int]
selectSnd l = [y | (_,y) <- l]

dotProd :: [Int] -> [Int] -> Int
dotProd l1 l2 = sum [ x * y | (x,y) <- zip l1 l2]