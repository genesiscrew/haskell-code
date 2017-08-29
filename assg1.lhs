>import Data.Char
>import System.IO 

>myFunc5:: Char -> Bool
>myFunc5 list = Data.Char.isSpace (list)

>--    helper function that recurses through characters and returns any character before a space 
>myFunc2 :: [Char] -> [Char] 
>myFunc2 [] = []
>myFunc2 (x:xs) | myFunc5 x == True = take 0 (x:xs)
>               | otherwise = [x] ++ myFunc2 xs 

>-- helper function that removes all unwanted characters using the elem function
>myFunc6 :: [Char] -> [Char]
>myFunc6 [] = []
>myFunc6 xs = [ x | x <- xs, not (x `elem` ",.?!-:;\"\'") ]

>-- main function that convert a line of string into a list of words 
>-- works recursing through the string while there are characters inside it, if so then
>-- it uses helper functions 2 to extract the word, helper function 6 to remove unwanted characters
>-- and finally appends the word the list of words using ++ command
>-- last point is the function is called recursively but the next recursion uses only the remaining >--text in the string, this is done by removing the length of characters equalling the current word >--from the list

>lineToWords  :: String -> [String]
>lineToWords [] = []
>lineToWords (x:xs) | x == ' ' = [(x:xs)]
>                   | otherwise = [myFunc6(myFunc2 (x:xs))] ++ lineToWords (drop (length (myFunc2 (x:xs))) xs)

>-- main function that convert a list of strings into a list of words,
>-- this is done by feeding each line to the linetowords function and apending the output
>-- to the next recursion which is fed the remaining lines
>-- finally recursion stops when list is empty throuhg guard conditional.

>linesToWords  :: [String] -> [String]
>linesToWords  [] = []
>linesToWords  (x:xs) | x /= [] = lineToWords x ++ linesToWords    xs



>-- lineToTuples linestoTuples and posOfWords were my attempt at qs 1c
>-- however i was unable to complete this task fully

>lineToTuples  :: String -> [(String, Int)]
>lineToTuples [] = []
>lineToTuples (x:xs) | myFunc2 (xs) == [] = [(myFunc2((x:xs)),(length (x:xs)) - (length $ myFunc2((x:xs))))]
>              | otherwise = [(myFunc6(myFunc2 (x:xs)),(length (x:xs)) - (length $ myFunc2((x:xs))))] ++ lineToTuples (drop (length (myFunc2 (x:xs))) xs)


>linestoTuples :: [String] -> [(String, Int)]
>linestoTuples [] = []
>linestoTuples (x:xs) | x /= [] = lineToTuples x ++ linestoTuples xs


>posOfWords :: [String] -> [(String)] 
>posOfWords (x:xs) = [ x | x <- xs ]



>-- helper function that receives a list of tuples containing the word and its position and a target word
>-- the function uses a conditional to check if it exist and in addition returns the position of
>-- first instance of word, in case it appears earlier in the data. 
>-- it continues recusing through list untill it doesnt find a matchm hence we have pattern to catch
>-- such errors  
>elemIndex :: [(String, Int)] -> String -> Int
>elemIndex  [] [] = error "i dont know"
>elemIndex  [] e = error "element not found"
>elemIndex (x:xs) e | e == (fst x) = snd x
>                   | otherwise = elemIndex xs e



>-- main function that is used to encode a line of string or lines of String(satisfies 3A and 3A+) 
>-- for the list of words it runs encodeline function and gets the first element using fst
>-- and then since x | x<- xs recurses through each element and we are running our function
>-- on each line we are getting multiple lists, hence i used concat command to combine all of the
>-- lists, i then run unique on that list to remove duplicates.
>-- for the numbers i run encodeOrder and retrieve the ordered numbers, again i use concat
>-- for some reason the output numbers were reversed so i had to use reverse on the output.

>encode :: [String] -> ([String], [Int]) 
>encode [] = ([],[])
>encode xs = ( unique $ concat[fst(encodeLine 0 x) | x <- xs] , numbers)
>                   where numbers = reverse $ snd $ encodeOrder ([],[]) (encodeList((encodeLine 0 (concat xs)))) 0 (encodeList((encodeLine 0 (concat xs))))


>-- thise function is similar to LintoWords except instead of return a list of string it returns
>-- a tuple consisting of a list of words and a list of their positions

>encodeLine :: Int -> String -> ([String],[Int])
>encodeLine _ [] = ([],[])
>encodeLine w (x:xs) | myFunc2 (x:xs) /= [] =  ([myFunc2 (x:xs)] ++ fst (encodeLine (w+1) (drop (length (myFunc2 (x:xs))) xs)),[w+1] ++ snd (encodeLine (w+1) (drop (length (myFunc2 (x:xs))) xs)))


>-- helper function that combines two list items into tuples, uses the zip function
>-- this was needed because  converting the data into a list of tuples made it easier to
>-- get the position value of each word easily

>encodeList :: ([String], [Int]) -> [(String,Int)]
>encodeList ([],[]) = []
>encodeList xs = zip (fst xs)  (snd xs)



>-- the aim of this function is to modify the order of the numbers
>-- the input consists of an empty tuple consisting of the words and the poditions which will be 
>-- the final output of the function.
>-- it also consists of an integer initialized to zero which will be used to number items 
>--  it will also consist of two copies of the original list of tuples of our data
>-- this was done because we need one list to recurse through, and another to be able to get elemIndex
>-- which would require the whole array since duplicates might exist.
>-- there are three conditionals, the first checks if word existed before, if it did then we number 
>-- it based on where it first appeared, second condition is for words that havent appeared and
>-- it also has two subconditions for the numbering based on variable r 
>-- variable r is equal to our counter if w is less or equal the element index we already have
>-- else it is equal to the original numbering. i arrived at this solution through intuition
>-- and through looking at the numbers, however i can not exactly explain why it works..
>-- the final condition happens only if we have recursed through all the items in our list of tuples, 
>-- hence it returns the tuple we have been appending in the first two conditions

>encodeOrder :: ([String], [Int])  -> [(String,Int)] -> Int -> [(String,Int)] -> ([String], [Int])
>encodeOrder ([],[]) [] _ [] = ([],[])
>encodeOrder z v _ [] = z
>encodeOrder (zs, ts) v w (x:xs) | fst x /= " " && (elemIndex v (fst x)) /=  (snd x) =  encodeOrder ((fst x:zs), ((elemIndex v (fst x)):ts)) v w xs
>                            | fst x /= " " = encodeOrder ((fst x:zs), (r:ts)) v (w+1) xs
>                            | otherwise = (zs, ts)
>                              where r | w <= (elemIndex v (fst x))  = (w+1)
>                                      | otherwise = (elemIndex v (fst x))


>-- helper function that returns only unique elements in a list using the filter function
>unique :: Eq a => [a] -> [a]
>unique []       = []
>unique (x : xs) = x : unique (filter (x /=) xs)


>-- test functions, run t1 1a t2 for 1b and t3 for question 3a and t4 is for 3b
>t1 = (lineToWords "For example, this sentence has 7 words!") 
>t2 = (linesToWords ["For example,", "in this sentence,", "there are 9 words."]) 
>t3 = (encode ["The more I learn, the more I know.","The more I know, the more I forget."]) 
>t4 = decode (encode ["The more I learn, the more I know.","The more I know, the more I forget."])


>-- this function takes the list of words and their positions and returns the original 
>decode :: ([String],[Int]) -> String
>decode (x,xs) = decodehelper x xs []


>-- helper function for the decode function 
>decodehelper :: [String] -> [Int] -> String -> String
>decodehelper x [] y     = y
>decodehelper x (w:xs) [] = decodehelper x xs (x !! (w-1))
>decodehelper x (w:xs) y  = decodehelper x xs (y ++ " " ++ (x !! (w-1)))




                     


                
