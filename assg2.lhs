>import Data.List


>makeSet :: (Eq a) => (Ord a) => [a] -> Set a
>data Set a = Set [a] deriving (Eq, Ord, Read, Show)

>-- the method makeSet creates a set. at first i was trying to make sure i remove duplicated
>-- using my own algorithm, after some research i discovered the nub function which makes it easy.
>-- i also used the sort function to make the set ordered. 

>makeSet [] = (Set [])
>makeSet xs = (Set $ sort $ nub xs)

>-- for the add method below i simply checked first whether element exists using has function,
>-- if it doesnt i just append it, otherwise i return the original list.

>add :: (Eq a) => a -> Set a -> Set a
>add a (Set xs) | has a (Set xs) == False = (Set (a:xs))
>               | otherwise = (Set xs)

>-- for the delete method below i used a simple list comprehension to return a set that doesnt include te required
>-- element

>del :: (Eq a) => a -> Set a -> Set a
>del a (Set xs) = (Set [ x | x <- xs, x /= a ])

>-- the function has below is used to check whether an element exists in a set. it works for both ordered and unordered. i used the pattern to return false if element not in set, and used recursion on the tail of set if first element does not equal the element we are looking for.
>-- sets. 

>has :: (Eq a) => a -> Set a -> Bool
>has a (Set []) = False
>has a (Set set) | a == y = True
>            | otherwise = has a (Set setx)
>            where y = head $ set
>                  setx = tail $ set


>--the function card below is quite self explanatory, it just returns the length of the Set.

>card :: Set a -> Int
>card (Set []) = 0
>card (Set xs) = length xs

>-- the method union below combines two sets. notice how patterns made life real easy, if any of the 
>-- sets is empty, union only returns the one that is not empty. if they are both full, then we simply
>-- combine the two lists using ++ and again use nub to remove duplicates.

>union :: (Eq a) => Set a -> Set a -> Set a
>union (Set x) (Set []) = (Set x)
>union (Set []) (Set y) = (Set y)
>union (Set x) (Set y) = (Set $ nub (y++x))

>-- the intersect method was little trickier than the ones before. again patterns made life real easy
>-- if any of the sets is empty, the intersect would also be empty. if both are full, then the logic was to append to list an element that exists in both ie element x is in Y, the elem function made this easy. the tricky part was to run the recursion on  the set type. the set type includes a list, and in order to access the list i had to use where command, which means i can declare (Set u) and then i can get access to the elements of the set. and set u is where the recursion happens which keeps appending items that are in both sets.

>-- 
>intersect :: (Eq a) => Set a -> Set a -> Set a
>intersect (Set x) (Set []) = (Set [])
>intersect (Set []) (Set y) = (Set [])
>intersect (Set (x:xs)) (Set y) | x `elem` y = (Set $ nub (x:u))
>                             | otherwise =  (Set $ nub u)
>                              where (Set u) = Main.intersect (Set xs) (Set y)



>equals :: (Eq a) => (Ord a) => Set a -> Set a -> Bool 
>equals (Set x) (Set y) | length x /= length y = False
>equals (Set []) (Set []) = True
>equals (Set (x:xs)) (Set (y:ys)) | (r:rs) /= (q:qs) = False
>                                 | otherwise =  u
>                              where u = Main.equals (Set xs) (Set ys)
>                                    (Set (r:rs)) = makeSet (x:xs)
>                                    (Set (q:qs)) = makeSet (y:xs)

>-- for the equals method below i wanted to try a new techniques since most of my functions rely on
>-- recursion. you can see that the implementation is a one liner and works for both ordered and >--unordered sets. for this function i used the any function which requires two inputs, a >--function to run on an element, and a list, and it returns only those elements that satisfy the >--requirements of the function, which in this case was == to first element of list a. hence it can >--work with ordered and >--unordered sets. since the output of this function is boolean, i assumed >--that we can AND the output of the any function with a recursion on the tail of x and the other >--list. i also had beforehand three patterns which made sense to me, which where that if one set is >--empty, then it should return false, and if both are empty then it should return true. the problem >--was that after checking for an element and running the recursion on the tail of list a while >--keeping list b in tact meant that the patterns returned false. after some thinking i realised that
>-- the sets do not have duplicates so if we find a match in set b we could just remove that element
>-- and if both sets are equals the recursion should end with two empty lists, hence satisfying one >--of the patterns. this showed my the elegance of haskell where you can use patterns, logic and math 
>-- to arrive at nice solutions.


>equalsU :: (Eq a) => Set a -> Set a -> Bool 
>equalsU (Set []) (Set []) = True
>equalsU (Set []) (Set y) = False
>equalsU (Set x) (Set []) = False
>equalsU (Set (x:xs)) (Set (y))  = any(x==) y && equalsU (Set xs) (Set $ (delete x y))


>-- the subset method below followed on from the same logic of equals method, except it was easier to >-- implement. the logic would be simple, we recurse through the tails of list a, and each time we use
>-- the elem function to check whether the head of list a is in list b. again i started thinking
>-- about patterns, logic dictated that an empty list would be a subset of any list, hence i added 
>-- these patterns. after some thinking i then realized that we should also check of list b is a 
>-- subset of list a. again, the power haskell was made evident, when i just OR'd the original code
>-- with similar code but applied in other direction, and it worked great after testing.  

>subset :: (Eq a) =>  Set a -> Set a -> Bool
>subset (Set []) (Set y) = True
>subset (Set x) (Set []) = True
>subset (Set (x:xs)) (Set (y:ys)) = (elem x (y:ys) && subset (Set xs) (Set (y:ys))) || (elem y (x:xs) && subset (Set ys) (Set (x:xs)))

>--the select method below i thought at the beginning would be hard to implement, but it turned out
>-- to be quite easy due to list comprehension, i passed the function to the filter and it worked 
>-- perfectly 

>select :: (a -> Bool) -> Set a -> Set a
>select vs (Set xs) = (Set  [x | x <- xs, (vs x)])


>perms :: (Eq a) => [a] -> [[a]]
>perms [] = [[]]
>perms xs = [x:ys | x <- xs, ys <- perms (delete x xs)]

>asc :: (Eq a) => (Ord a) => [a] -> Bool
>asc [] = True
>asc (x:xs) | xs /= [] && x > w = False 
>           | otherwise = asc xs
>           where w = head xs 


>sorti :: (Ord a) => [a] -> [a]
>sorti l = head [ s | s <- perms l, asc s]



>data Graph a b = Empty | Graph [a] [(a,b,a)] deriving (Eq, Ord, Read, Show)

>data Path a b = Path [(a,b,a)] | Nothing deriving (Eq, Ord, Read, Show)

>makeGraph :: (Eq a) => (Eq b) => (Ord a) => ([a], [(a,b,a)]) -> Graph a b
>makeGraph (q,(x:xs)) | (not $ (fst3 x) `elem` q) || (not $ (thrd3 x) `elem` q) || ((containsU (x:xs) (x:xs) ) == True)  || ((containsU q q ) == True)  = error "invalid data format"
>                     |  xs /= [] = (Graph q (x:s))
>                     | otherwise = (Graph q (x:xs))   
>              where (Graph d s) = makeGraph (q,xs)   

>fst3 :: (a, b, c) -> a
>fst3 (x, _, _) = x

>snd3 :: (a, b, c) -> b
>snd3 (_,x, _) = x

>thrd3 :: (a, b, c) -> c
>thrd3 (_, _,x) = x

>contains :: (Eq a) => a -> [a] -> Bool
>contains a [] = False
>contains a (x:xs) | x == a = True
>            | otherwise = contains a xs

>containsU :: (Eq a) => [a] -> [a] -> Bool
>containsU [] [] = False
>containsU (y:ys) (l:ls) | ls /= [] && (contains y ls) == True = True
>            | otherwise = containsU ys ls

>predecessors :: (Eq a) =>  Graph a b -> a -> Set a 
>predecessors (Graph v xs) y = (Set  [x | x <- v, (predFind xs x y) == True ])


>predFind :: (Eq a) => (Eq c) => [(a,b,c)] -> a -> c -> Bool
>predFind [] f y = False
>predFind (x:xs) f y | (fst3 x) == f && (thrd3 x) == y = True
>                  | otherwise = predFind xs f y


>successors :: (Eq a) =>  Graph a b -> a -> Set a 
>successors  (Graph v xs) y = (Set  [x | x <- v, (sucFind xs x y) == True ])


>sucFind :: (Eq a) => (Eq c) => [(a,b,c)] -> c -> a -> Bool
>sucFind [] f y = False
>sucFind (x:xs) f y | (fst3 x) == y && (thrd3 x) == f = True
>                   | otherwise = sucFind xs f y


>sumGraph (Graph v xs) ys  | ys /= [] =  (length ws) + (sumGraph (Graph v xs) wt) 
>                          | otherwise = 0    
>                           where (Set ws) = (successors (Graph v xs) q) 
>                                 q = head ys
>                                 wt = tail ys


>isConnected :: (Eq a) => Graph a b -> a -> Bool
>isConnected (Graph v xs) h | (contains h v) && (sumGraph (Graph v xs) v) == length xs && (length tr) == 0 = True
>                          | otherwise = False
>                          where (Set tr) = (predecessors (Graph v xs) h) 


>findPath :: (Eq a) =>  Graph a b -> a -> a -> Maybe (Path a b)
>findPath (Graph [] xs) y z = Prelude.Nothing
>findPath (Graph v xs) y z | wx /= [] && (contains z wx) == True = Just (Path [(y, findLabel (Graph v xs) y z ,z)])
>                          | wx == [] = Prelude.Nothing
>                          | otherwise = pathUnion (Path el) ed
>                          where (Set wx) = successors (Graph v xs) y 
>                                (Just ed) = findPath (Graph v xs) (head wx) z  
>                                el = [(y, findLabel (Graph v xs) y (head wx) ,(head wx))]


>findLabel :: (Eq a) => Graph a b -> a -> a -> b
>findLabel (Graph v (x:xs)) y z | (fst3 x) == y && (thrd3 x) == z =  (snd3 x)
>                               | otherwise = findLabel (Graph v xs) y z   


>pathUnion :: (Eq a) => Path a b -> Path a b -> Maybe (Path a b)
>pathUnion (Path y) (Path []) = Just (Path y)
>pathUnion (Path []) (Path x) = Just (Path x)
>pathUnion (Path y) (Path x) = Just (Path (y++x))
   
 
    


       
                 




