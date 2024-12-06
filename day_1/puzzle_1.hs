-- can I split the string I get or print out all the characters in one line and build up the two
-- now have a list of [['left Int', 'right Int']]
-- split those into two lists a left and right
-- can run sort on the lists and get them in order
-- then maybe zip them together or grab each pairing and subtract them to get the difference
import Data.List

-- part 1
-- main :: IO Int
-- main = do
--  contents <- readFile "puzzle_1_input.txt"
--  let parsed = [words s | s <- lines contents]
--  let leftVals = sort [read left :: Int | [left, _] <- parsed]
-- let rightVals = sort [read right :: Int | [_, right] <- parsed]
--  let zipped = zip leftVals rightVals
--  return (sum [abs (a - b) | (a, b) <- zipped])

-- part 2
-- for this part everytime we encounter a number we need to add its similarity score
-- lets say we have 3 3s in the left list and 3 3s in the right list
-- we only need the frequency of a number on the right list
-- and also a frequency of the number on the left list
-- so for this we know that there are 3 3s in the left and 3 3s on the right
-- for the number 3 we have a similarity score of 9
-- but we have 3 3s so our total similarity score for 3s is 27
-- so for the left list we maintain the frequency and the right list a frequency as well
-- when we compute we sum up the frequency of values we get from the left and right
-- in this case our equation is for each frequency of left * similarity score of value on the right
-- there can be empty values on the right
--
main :: IO Int
main = do
  contents <- readFile "puzzle_1_input.txt"
  let parsed = [words s | s <- lines contents]
  let leftVals = [read left :: Int | [left, _] <- parsed]
  let rightVals = [read right :: Int | [_, right] <- parsed]
  -- want to find the frequency of lefts in leftVals and frequency of lefts in the rightVals as a tuple (10000, 1, 2)
  let vals = [(left, length (elemIndices left leftVals), length (elemIndices left rightVals)) | left <- leftVals]
  return (foldl' (\acc (val, freq, score) -> acc + (val * freq * score)) 0 vals)
