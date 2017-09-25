
--What needs to be done:
--Create a data structure that holds a 2d array with a free or blocked value.
--In the end, the function spits out a path
--The path contains every step taken from your starting point to your end point
--Use patternmatching of some sort to find where the Start and End are.



data Square a b = Wall a b | Free a b | Start a b | End a b deriving (Show, Eq)
data Priority = First | Midleft | Midright | Last

type Grid = [Square Int Int]
type Path = Grid


thegrid :: Grid
thegrid = [Free 0 0, Free 1 0, Free 2 0, Free 3 0, Free 4 0, Free 5 0, Free 6 0,
		   Free 0 1, Free 1 1, Free 2 1, Wall 3 1, Free 4 1, Free 5 1, Free 6 1,
		   Free 0 2, Free 1 2, Free 2 2, Wall 3 2, Free 4 2, Free 5 2, Free 6 2,
		   Free 0 3, Free 1 3, Free 2 3, Wall 3 3, Free 4 3, Free 5 3, Free 6 3,
		   Start 0 4, Free 1 4, Free 2 4, Wall 3 4, Free 4 4, Free 5 4, End 6 4,
		   Free 0 5, Free 1 5, Free 2 5, Wall 3 5, Free 4 5, Free 5 5, Free 6 5,
		   Free 0 6, Free 1 6, Free 2 6, Wall 3 6, Free 4 6, Free 5 6, Free 6 6]

defpath :: Path
defpath = []

gridsearch :: Grid -> Path -> Path
gridsearch xs ys = findadj First xs (findstart xs) 0


--Recursive function needs to be defined with base case of empty list of adj squares or of End square.
--Call recursive function on each adj square of each adjacent square. Append each path and cost to a list, find the path with the least associated cost.


findadj :: Priority -> Grid -> Path -> Int -> Path
findadj _ xs y@[_, End a b, _] sum = y
findadj First xs p@(y:ys) sum = let filfunc = filter (isadj y) xs in if sum > 24 then take 500 $ repeat (Wall 0 0) else if (filter (isadj $head filfunc) xs) == ys then take 500 $ repeat (Wall 0 0) else firstend [findadj First xs filfunc (sum + 1), findadj Midright xs filfunc (sum + 1), findadj Midleft xs filfunc (sum + 1), findadj Last xs filfunc (sum + 1)]
findadj Last xs ys sum = let filfunc = filter (isadj (last ys)) xs in if sum > 24 then take 500 $ repeat (Wall 0 0) else if (filter (isadj $last filfunc) xs) == ys then take 500 $ repeat (Wall 0 0) else firstend [findadj First xs filfunc (sum + 1), findadj Midright xs filfunc (sum + 1), findadj Midleft xs filfunc (sum + 1), findadj Last xs filfunc (sum + 1)]
findadj Midright xs ys sum = let filfunc = filter (isadj (splitdirectional Midright ys)) xs in if sum > 24 then take 500 $ repeat (Wall 0 0) else if (filter (isadj $splitdirectional Midright filfunc) xs) == ys then take 500 $ repeat (Wall 0 0) else firstend [findadj First xs filfunc (sum + 1), findadj Midright xs filfunc (sum + 1), findadj Midleft xs filfunc (sum + 1), findadj Last xs filfunc (sum + 1)]
findadj Midleft xs ys sum = let filfunc = filter (isadj (splitdirectional Midleft ys)) xs in if sum > 24 then take 500 $ repeat (Wall 0 0) else if (filter (isadj $splitdirectional Midleft filfunc) xs) == ys then take 500 $ repeat (Wall 0 0) else firstend [findadj First xs filfunc (sum + 1), findadj Midright xs filfunc (sum + 1), findadj Midleft xs filfunc (sum + 1), findadj Last xs filfunc (sum + 1)]
findadj _ xs ys _= [Start 4 0]
--original line was :: findadj xs $ filter (isadj y) xs



isadj :: Square Int Int -> Square Int Int -> Bool
isadj (Free a b) (Wall c d) = False
isadj (Start a b) (Wall c d) = False
isadj (Free a b) (Free c d) = if (abs (a - c) > 0) && (abs (b - d) > 0) || (abs (a - c) > 1) || (abs (b - d) > 1)  then False else if Free a b == Free c d then False else True
isadj (Start a b) (Free c d) = if (abs(a - c) > 0) && (abs (b - d) > 0) || (abs (a - c) > 1) || (abs (b - d) > 1) then False else True
isadj (Free a b) (End c d) = if (abs(a - c) > 0) && (abs (b - d) > 0) || (abs (a - c) > 1) || (abs (b - d) > 1) then False else True
--isadj (End a b) (Free c d) = if (abs(a - c) > 0) && (abs (b - d) > 0) || (abs (a - c) > 1) || (abs (b - d) > 1) then False else True
isadj _ _ = False

 	
findstart :: Grid -> Path
findstart xs = filter isStart xs
--Note: This solution assumes that the Grid will only have one Start.



findend :: Grid -> Path
findend xs = filter isEnd xs

isEnd :: Square a b -> Bool
isEnd (End _ _) = True
isEnd _ = False

isStart :: Square a b -> Bool
isStart (Start _ _) = True
isStart _ = False

isFree :: Square a b -> Bool
isFree (Wall _ _) = False
isFree _ = True



xor :: Bool -> Bool -> Bool
xor x y | x == True && y == False = True
		| x == False && y == True = True
		| otherwise = False


splitdirectional :: Priority -> Path -> Square Int Int
splitdirectional Midleft xs = if length xs `mod` 2 == 0 then xs!!((length xs) `quot` 2 - 1) else xs!!(length xs `quot` 2)
splitdirectional Midright xs = if length xs `mod` 2 == 0 then xs!!((length xs) `quot` 2) else xs!!(length xs `quot` 2)



firstend :: [[Square Int Int]] -> [Square Int Int]
firstend [_, y@[_, End a b, _], _] = y
firstend ys = head ys



