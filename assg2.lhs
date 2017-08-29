>import Data.List
>makeSet :: (Eq a) => (Ord a) => [a] -> Set a
>data Set a = Set [a] deriving (Eq, Ord, Read, Show)
>--type Set a = [a]
>makeSet [] = (Set [])
>makeSet xs = (Set $ sort $ nub xs)

>add :: (Eq a) => a -> Set a -> Set a
>add a (Set xs) | has a (Set xs) == False = (Set (a:xs))
>               | otherwise = (Set xs)

>del :: (Eq a) => a -> Set a -> Set a
>del a (Set xs) = (Set [ x | x <- xs, x /= a ])

>has :: (Eq a) => a -> Set a -> Bool
>has a (Set []) = False
>has a (Set set) | a == y = True
>            | otherwise = has a (Set setx)
>            where y = head $ set
>                  setx = tail $ set

>card :: Set a -> Int
>card (Set []) = 0
>card (Set xs) = length xs

>union :: (Eq a) => Set a -> Set a -> Set a
>union (Set x) (Set []) = (Set x)
>union (Set []) (Set y) = (Set y)
>union (Set x) (Set y) = (Set $ nub (y++x))

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

>equalsU :: (Eq a) => Set a -> Set a -> Bool 
>equalsU (Set []) (Set []) = True
>equalsU (Set []) (Set y) = False
>equalsU (Set x) (Set []) = False
>equalsU (Set (x:xs)) (Set (y))  = any(x==) y && equalsU (Set xs) (Set $ (delete x y))

>subset :: (Eq a) =>  Set a -> Set a -> Bool
>subset (Set []) (Set y) = True
>subset (Set x) (Set []) = True
>subset (Set (x:xs)) (Set (y:ys)) = (elem x (y:ys) && subset (Set xs) (Set (y:ys))) || (elem y (x:xs) && subset (Set ys) (Set (x:xs))) 

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
>type Node = Int
>--type Adj b = [(b, Node)]
>--type Context a b = (Adj b, Node, a, Adj b)
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
>-- 
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
>                            | otherwise = punion (Path el) ed
>                          where (Set wx) = successors (Graph v xs) y 
>                                (Just ed) = findPath (Graph v xs) (head wx) z  
>                                el = [(y, findLabel (Graph v xs) y (head wx) ,(head wx))]
>findLabel :: (Eq a) => Graph a b -> a -> a -> b
>--findLabel (Graph v []) y z = Prelude.Nothing 
>findLabel (Graph v (x:xs)) y z | (fst3 x) == y && (thrd3 x) == z =  (snd3 x)
>                               | otherwise = findLabel (Graph v xs) y z   

>punion :: (Eq a) => Path a b -> Path a b -> Maybe (Path a b)
>punion (Path y) (Path []) = Just (Path y)
>punion (Path []) (Path x) = Just (Path x)
>punion (Path y) (Path x) = Just (Path (y++x))
   
 
    


       
                 




