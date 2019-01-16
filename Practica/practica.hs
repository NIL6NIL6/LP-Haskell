exampleSet::Kd2nTree Point3d
exampleSet = buildIni [([3,-1,2.1],[1,3]),([3,5.1,0],[2]),([3.5,0,2.1],[3]),([1.8,1.1,-2],[1,2]),([1.5,8,1.5],[1]),([3.5,2.8,3.1],[1,2]),([3.1,3.8,4.8],[1,3]),([3,-1.7,3.1],[1,2,3]),([4,5.1,3.8],[2]),([3.3,2.8,2.5],[3])]

------------------------------------------------------------------------

-------------------OPERACIONS GENÈRIQUES--------------------------------

merge :: (Ord p) => [p] -> [p] -> [p]
merge [] [] = []
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys)
	| x < y = x:(merge xs (y:ys))
	| otherwise = y:(merge (x:xs) ys)

msort :: (Ord p) => [p] -> [p]
msort [] = []
msort [x] = [x]
msort xs = merge (msort (take (div (length xs) 2) xs)) (msort (drop (div (length xs) 2) xs))

------------------------------------------------------------------------

----------------------DEFINICIONS DE POINT------------------------------

class Point p where 
	sel :: Int -> p -> Double
	dim :: p -> Int
	child :: p -> p -> [Int] -> Int
	dist :: p -> p -> Double
	list2Point :: [Double] -> p
	ptrans :: [Double] -> p -> p
	
	child p1 p2 l = bin2Int $ f p1 p2 l
		where
		bin2Int l = foldl (\x y -> x*2 + y) 0 l
		f p1 p2 [] = []
		f p1 p2 (x:xs)
			| (sel x p2) <= (sel x p1) = 0:(f p1 p2 xs)
			| otherwise = 1:(f p1 p2 xs)
	
	dist p1 p2 = sqrt $ foldl (+) 0 $ f p1 p2
		where
		f p1 p2 = zipWith (\x y -> (x - y)*(x - y)) (g p1 (dim p1)) (g p2 (dim p2))
			where
			g p 0 = []
			g p n = (g p (n-1))++((sel n p):[])
			
	ptrans l p = list2Point [x | i <- take (dim p) $ iterate (+1) 1, let x = (sel i p) + (l !! (i-1))]

------------------------------------------------------------------------

---------------------DEFINICIONS DE POINT3D-----------------------------
			
data Point3d = Point3d Double Double Double

instance Point Point3d where
	sel i (Point3d x y z) 
		| i == 1 = x
		| i == 2 = y
		| i == 3 = z
	dim p = 3
	list2Point (x:(y:(z:[]))) = Point3d x y z

instance Show Point3d where
	show (Point3d x y z) = show (x,y,z)
	
instance Eq Point3d where
	(Point3d x y z) == (Point3d a b c) = (x == a) && (y == b) && (z == c)
	
instance Ord Point3d where
	(Point3d x y z) <= (Point3d a b c) = (x < a) || ((x == a) && (y < b)) || ((x == a) && (y == b) && (z <= b))

------------------------------------------------------------------------

-------------------DEFINICIONS DE KD2NTREE------------------------------

data Kd2nTree p = Kd2nTree p [Int] [Kd2nTree p] | Empty

instance (Point p, Ord p) => Eq (Kd2nTree p) where
	Empty == Empty = True
	Empty == _ = False
	_ == Empty = False
	t1 == t2 = (msort $ map fst $ get_all t1) == (msort $ map fst $ get_all t2)

instance (Point p, Show p) => Show (Kd2nTree p) where
	show Empty = ""
	show (Kd2nTree p l a) = (show p)++" "++(show l)++(f a 0 0)
		where
		f [] _ _ = ""
		f (Empty:(xs)) i j = f xs i (j+1)
		f ((Kd2nTree p2 l2 a2):xs) i j = ['\n']++(tab i)++"<"++(show j)++">\
		\ "++(show p2)++" "++(show l2)++(f a2 (i+1) 0)++(f xs i (j+1))
			where
			tab i = ['\t' | x <- (take i [1..])]

------------------------------------------------------------------------

instance Functor (Kd2nTree) where
	fmap g Empty = Empty
	fmap g (Kd2nTree p l a) = Kd2nTree (g p) l $ map (fmap g) a

instance Applicative (Kd2nTree) where
	pure = return
	(<*>) Empty _ = Empty
	(<*>) _ Empty = Empty
	(<*>) (Kd2nTree f l1 a) (Kd2nTree p l2 b) = (Kd2nTree (f p) l2 (zipWith (<*>) a b)) 
	
instance Monad (Kd2nTree) where
	return p = Kd2nTree p [] []
	Empty >>= _ = Empty
	(Kd2nTree p l a) >>= k = f (k p)
		where
		f (Kd2nTree p2 [] []) = (Kd2nTree p2 l $ map (\x -> x >>= k) a)
		f t = t
		
------------------------------------------------------------------------

---------------------OPERACIONS SOBRE KD2NTREE--------------------------

insert :: (Point p) => Kd2nTree p -> p -> [Int] -> Kd2nTree p
insert Empty p l = Kd2nTree p l [Empty | x <- (take (2^(length l)) [1..])]
insert (Kd2nTree p l a) new_p new_l = Kd2nTree p l (insPos a $ child p new_p l)
	where
	insPos (x:xs) t
		| (t /= 0) = x:(insPos xs (t-1))
		| (t == 0) = (insert x new_p new_l):xs

build :: (Point p) => [(p,[Int])] -> Kd2nTree p
build l = foldl (\x (a,b) -> insert x a b) Empty l

buildIni :: (Point p) => [([Double],[Int])] -> Kd2nTree p
buildIni l = foldl (\x (a,b) -> insert x (list2Point a) b) Empty l

get_all :: (Point p) => Kd2nTree p -> [(p,[Int])]
get_all Empty = []
get_all (Kd2nTree p l a) = (p,l):(concat [get_all x | x <- a])

remove :: (Point p, Eq p) => Kd2nTree p -> p -> Kd2nTree p
remove Empty _ = Empty
remove (Kd2nTree p l a) p2
	| p == p2 = foldl (\x (a,b) -> insert x a b) Empty $ tail $ get_all (Kd2nTree p l a)
	| otherwise = (Kd2nTree p l $ remPos a $ child p p2 l)
		where
		remPos (x:xs) t
			| (t /= 0) = x:(remPos xs (t-1))
			| otherwise = (remove x p2):xs

contains :: (Point p, Eq p) => Kd2nTree p -> p -> Bool
contains Empty _ = False
contains (Kd2nTree p l a) p2
	| p == p2 = True
	| otherwise = contains (a !! (child p p2 l)) p2
	
nearest :: (Point p, Ord p) => Kd2nTree p -> p -> p
nearest (Kd2nTree p l a) p2 = foldr (\x y -> if ((dist x p2) < (dist y p2)) then x else y) p $ map (\x -> nearest x p2) [x | x <- a, x /= Empty]
	
allinInterval :: (Point p, Ord p) => Kd2nTree p -> p -> p -> [p]	
allinInterval Empty _ _ = []
allinInterval (Kd2nTree p l a) p1 p2
	| (p1 <= p) && (p <= p2) = foldl (\x y -> merge x $ allinInterval y p1 p2) [p] a
	| otherwise = foldl (\x y -> merge x $ allinInterval y p1 p2) [] a

------------------------------------------------------------------------

translation :: Point t => [Double] -> Kd2nTree t -> Kd2nTree t
translation l t = fmap (ptrans l) t

kfilter :: Point p => (p -> Bool) -> Kd2nTree p -> Kd2nTree p
--kfilter f t = do { x <- t; if (f x) then (\(Kd2nTree p l a) -> Kd2nTree x l $ map (kfilter f) a) t else (\(Kd2nTree p l a) -> kfilter f $ build $ concat $ map (get_all) a) t}
kfilter f Empty = Empty
kfilter f t@(Kd2nTree p l a) = if (f p) then Kd2nTree p l (do { x <- a; [kfilter f x]})
								else foldl (union) Empty $ map (kfilter f) a

union :: (Point p) => Kd2nTree p -> Kd2nTree p -> Kd2nTree p
union t1 t2 = build $ (get_all t1)++(get_all t2)

------------------------------------------------------------------------

-------------------------FI DEL DOCUMENT--------------------------------

------------------------------------------------------------------------
