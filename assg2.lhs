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

>-- for the equalsU method below i wanted to try a new techniques since most of my functions rely on
>-- recursion. you can see that the implementation is a one liner and works for both ordered and >--unordered sets. for this function i used the any function which requires two inputs, a >--function to run on an element, and a list, and it returns only those elements that satisfy the >--requirements of the function, which in this case was == to first element of list a. hence it can >--work with ordered and >--unordered sets. since the output of this function is boolean, i assumed >--that we can AND the output of the any function with a recursion on the tail of x and the other >--list. i also had beforehand three patterns which made sense to me, which where that if one set is >--empty, then it should return false, and if both are empty then it should return true. the problem >--was that after checking for an element and running the recursion on the tail of list a while >--keeping list b in tact meant that the patterns returned false. after some thinking i realised that
>-- the sets do not have duplicates so if we find a match in set b we could just remove that element
>-- and if both sets are equals the recursion should end with two empty lists, hence satisfying one >--of the patterns. this showed me the elegance of haskell where you can use patterns, logic and math 
>-- to arrive at nice solutions.


>equalsU :: (Eq a) => Set a -> Set a -> Bool 
>equalsU (Set []) (Set []) = True
>equalsU (Set []) (Set y) = False
>equalsU (Set x) (Set []) = False
>equalsU (Set (x:xs)) (Set (y))  = any(x==) y && equalsU (Set xs) (Set $ (delete x y))


>-- the subset method below followed on from the same logic of equals method, except it was easier to >-- implement. the logic would be simple, we recurse through the tails of list a, and each time we use
>-- the elem function to check whether the head of list a is in list b. again i started thinking
>-- about patterns, logic dictated that an empty list would be a subset of any list, hence i added 
>-- these patterns. after some thinking i then realized that we should also check if list b is a 
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

>-- the perms method below took quite some thinking before i actually implemented it. i had tried
>-- to do it using recursion but got stuck on that implementation. 

>perms :: (Eq a) => [a] -> [[a]]
>perms [] = [[]]
>perms xs = [x:ys | x <- xs, ys <- perms (delete x xs)]

>-- the asc was more straightforward and easy to figure out. i decided to recurse through the entire >-- list checking if each element is more than the next one, if the list is ascending, then the >--recursion would end up with an empty list, and the pattern would be caught triggering the true case
>-- otherwise if any element is larger than its successor the recursion returns False. on slight issue
>-- was evident was that at the last element, there should be no comparison, i easily fixed that by
>-- adding an additional condition that the tail is not empty.

>asc :: (Eq a) => (Ord a) => [a] -> Bool
>asc [] = True
>asc (x:xs) | xs /= [] && x > w = False 
>           | otherwise = asc xs
>           where w = head xs 


>sorti :: (Ord a) => [a] -> [a]
>sorti l = head [ s | s <- perms l, asc s]



>data Graph a b = Empty | Graph [a] [(a,b,a)] deriving (Eq, Ord, Read, Show)

>data Path a b = Path [(a,b,a)] | Nothing deriving (Eq, Ord, Read, Show)


>-- a note about part 3. i believe this was the most challenging part, i managed to do most of the 
>-- functions, however I did not take into account graphs that contain cycles, so my functions do not >-- work for cycles.

>makeGraph :: (Eq a) => (Eq b) => (Ord a) => ([a], [(a,b,a)]) -> Graph a b
>makeGraph (q,(x:xs)) | (not $ (fst3 x) `elem` q) || (not $ (thrd3 x) `elem` q) || ((containsU (x:xs) (x:xs) ) == True)  || ((containsU q q ) == True)  = error "invalid data format"
>                     |  xs /= [] = (Graph q (x:s))
>                     | otherwise = (Graph q (x:xs))   
>              where (Graph d s) = makeGraph (q,xs)   

>-- helper function that returns the first element in a tuple of three

>fst3 :: (a, b, c) -> a
>fst3 (x, _, _) = x


>-- helper function that returns the second element in a tuple of three

>snd3 :: (a, b, c) -> b
>snd3 (_,x, _) = x


>-- helper function that returns the third element in a tuple of three

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

>-- the function predecessors took something thinking. after some analysis, i realized that
>-- the output of the function is just a subset of nodes of on the inputs which is a set of of
>-- all nodes in the graph. hence i decided to use list comprehension to run through all the
>-- items in the input node set, and i need a way to check if each node is a predecessor of the given >- node, so i needed another function that returns a boolean, and made predFind, if it returned true
>-- then the list comprehension filter would allow it to be added 

>predecessors :: (Eq a) =>  Graph a b -> a -> Set a 
>predecessors (Graph v xs) y = (Set  [x | x <- v, (predFind xs x y) == True ])


>-- regarding the predFind helper function below developing predFind was key to 
>-- make my idea to work. it would take a list of edges and simply
>-- check if the last element in edge is equal to our given node and the first element in edge
>-- is equal to a certain value, which represents all the other elements in our graph. since predfind 
>-- is run in every iteration of the list comprehension we are getting all values of x and comparing 
>-- them to our required node. again i am using recursion as the most preferred way. this is mostly 
>-- because i am still uncomfortable with how maps and folds work. as you can see, all my solutions
>-- are highly dependant on recursion which i find convenient. finally, i am again using patterns
>-- to make the solution elegant, if the list of edges is empty then false is returned

>predFind :: (Eq a) => (Eq c) => [(a,b,c)] -> a -> c -> Bool
>predFind [] f y = False
>predFind (x:xs) f y | (fst3 x) == f && (thrd3 x) == y = True
>                  | otherwise = predFind xs f y

>--regarding the successor function below, after getting predecessor to work, successors was straightforward to implement as it works
>-- exactly the same way but the direction is just reversed.

>successors :: (Eq a) =>  Graph a b -> a -> Set a 
>successors  (Graph v xs) y = (Set  [x | x <- v, (sucFind xs x y) == True ])

>--regarding the sucFind helper function below, again, it is a replica of predfind except the nodes are reversed.  

>sucFind :: (Eq a) => (Eq c) => [(a,b,c)] -> c -> a -> Bool
>sucFind [] f y = False
>sucFind (x:xs) f y | (fst3 x) == y && (thrd3 x) == f = True
>                   | otherwise = sucFind xs f y

>--sumGraph function, which is a helper function to isConnected is quite interesting, in the beggining it was supposed to actually be the actual isConnected
>--function. the idea i had before coding was that i was going to use the successors function to find out the succesors of the
>-- starting node, and then I wanted to recurse through each successor and run the same function on each one. i had difficulty 
>-- implementing this idea because at that stage i still hadnt formulated a plan to address the issue. i then analysed the problem
>-- on a piece of paper, and realized that given a list of nodes and edges, if the graph is connected then the length of given list >-- of edges should be equal to the sum of successors for each node in the graph. after this important finding, i needed to first
>-- calculate the number of successors for each node in graph. hence sumGraph was made. the idea was to pass to sumGraph a graph 
>-- and a list of nodes, and to incrementally sum the length of the result of successors function. on each recursion of sumGraph
>-- succesors is applied to an element in the list of nodes. again, as explained before, where function made it easy to access
>-- the set elements and to assign elements.   

>sumGraph (Graph v xs) ys x | ys /= [] && (length ws) > 1 =   er + (sumGraph (Graph v xs) [wt] x) + (sumGraph (Graph v xs) (tail ws) x)
>                           | (length (ws)) == 1 = (sumGraph (Graph v xs) [wt] er)
>                           | otherwise = x    
>                           where (Set ws) = (successors (Graph v xs) q) 
>                                 q = head ys
>                                 wt | ws /= [] = head ws
>                                 er = x+(length ws)


>--outlier (Graph v xs) ys  | ys /= [] && ws /= [] =  (length ws) && (outlier (Graph v xs) wt) 
>--                          | otherwise = 0    
>--                           where (Set ws) = (successors (Graph v xs) q) 
>--                                 q = head ys
>--                                 (Set wv) = (predecessors (Graph v xs) q)
>--                                 wt = tail ys

>-- the isConnected function below is highly dependant on the sumGraph function. given the output of sumGraph,
>-- checking whether the graph is connected required only to check whether the sum of successors is equal to the
>-- length of edges list in the graph.  

>isConnected :: (Ord a) => (Eq a) => Graph a b -> a -> Bool
>isConnected (Graph v xs) h | (h `elem` v) && (sumGraph (Graph (sort $ v) xs) [h] 0) == ((length xs)) && (length tr) == 0 = True
>                          | otherwise = False
>                          where (Set tr) = (predecessors (Graph v xs) h) 


>findPath :: (Eq a) =>  Graph a b -> a -> a -> Maybe (Path a b)
>findPath (Graph [] xs) y z = Prelude.Nothing
>findPath (Graph v xs) y z | wx /= [] && (z `elem` wx) == True = Just (Path [(y, findLabel (Graph v xs) y z ,z)])
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

>findPathLabel :: (Eq a) => Graph a b -> a -> a -> Maybe [b]
>findPathLabel (Graph [] xs) y z = Prelude.Nothing
>findPathLabel (Graph v xs) y z | wx /= [] && (z `elem` wx) == True = Just ([findLabel (Graph v xs) y z ])
>                          | wx == [] = Prelude.Nothing
>                          | otherwise = Just(el ++ ed)
>                          where (Set wx) = successors (Graph v xs) y 
>                                (Just ed) = findPathLabel (Graph v xs) (head wx) z  
>                                el = [findLabel (Graph v xs) y (head wx)]

   
 
    


       
                 




