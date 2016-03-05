{-|
 - Module       : ReductionGrammar
 - Description  : Normal-order reduction grammars.
 - Copyright    : (c) Maciej Bendkowski, 2016
 - Maintainer   : maciej.bendkowski@tcs.uj.edu.pl
 - Stability    : experimental
 -}
module ReductionGrammar where

    -- | Normal trees.
    data Tree = S | K | C | R Int
              | App Tree Tree
                deriving (Eq,Ord)
    
    instance Show Tree where
        showsPrec _ S = (:) 'S'
        showsPrec _ K = (:) 'K'
        showsPrec _ C = (:) 'C'
        showsPrec _ (R n) = (++) "R_" . shows n
        showsPrec _ (App e t @ (App _ _)) =
            shows e . (:) '(' . shows t . (:) ')'
        showsPrec _ (App e t) = shows e . shows t
    
    -- | Grammar production.
    data Production = ProdS [Tree]
                    | ProdK [Tree]
                        deriving (Eq,Ord)
    
    showsList :: [Tree] -> ShowS
    showsList [] = (++) ""
    showsList (t @ (App _ _) : ts) = (:) '(' .
        shows t . (:) ')' . showsList ts
    showsList (t : ts) = shows t . showsList ts

    instance Show Production where
        showsPrec _ (ProdS ts) = (:) 'S' . showsList ts
        showsPrec _ (ProdK ts) = (:) 'K' . showsList ts
   
    -- | Transforms a production into an equivalent tree.
    toTree :: Production -> Tree
    toTree p = case p of
        (ProdK as) -> toTree' K as
        (ProdS as) -> toTree' S as
        where
            toTree' = foldl App

    -- | Returns a list of tree arguments.
    args :: Tree -> [Tree]
    args a = reverse $ args' a
        where
            args' (App lt rt) = rt : args' lt
            args' _ = []

    -- | Returns the starting combinator.
    starting :: Tree -> Tree
    starting (App lt _) = starting lt
    starting x = x
   
    -- | Checks whether two trees are similar.
    similar :: Tree -> Tree -> Bool
    similar lt rt = s == s' && length ags == length ags'
        where
            (s, ags) = (starting lt, args lt)
            (s', ags') = (starting rt, args rt)

    -- | Checks whether the first tree rewrites to the second one.
    rew :: Tree -> Tree -> Bool
    rew S S = True
    rew S _ = False
    rew K K = True
    rew K _ = False
    rew C _ = True
    rew (R n) (R m) = n == m
    rew (R n) rt = or [toTree lt `rew` rt
                        | lt <- productions $
                          reductionGrammars !! n]
    rew t @ (App _ _) t'
        |  similar t t' = and $ zipWith
            rew (args t) (args t')
        | otherwise = False

    mesh :: [Tree] -> [Tree] -> [[Tree]]
    mesh (x : xs) (y : ys)
        | x `rew` y = [y] : mesh xs ys
        | y `rew` x = [x] : mesh xs ys
        | otherwise = meshSet x y : mesh xs ys
    mesh [] [] = []

    build :: Tree -> [[Tree]] -> [Tree]
    build s = map (App s . foldl1 App)

    -- | Given two trees, constructs thier common mesh set.
    meshSet :: Tree -> Tree -> [Tree]
    meshSet (R n) t @ (App _ _) = concatMap 
        (\g -> toTree g `meshSet` t) $
        productions (reductionGrammars !! n)
    meshSet t @ (App _ _) (R n) = concatMap 
        (\g -> t `meshSet` toTree g) $
        productions (reductionGrammars !! n)
    meshSet lt @ (App _ _) rt @ (App _ _)
        | similar lt rt = build (starting lt) 
            $ sequence (mesh ags ags')
        | otherwise = []
        where
            ags = args lt
            ags' = args rt
    meshSet _ _ = []

    -- | Given two trees, constructs thier rewriting set.
    rewritingSet :: Tree -> Tree -> [Tree]
    rewritingSet _ S = []
    rewritingSet _ K = []
    rewritingSet lt C = [App C lt]
    rewritingSet lt (R n) = concatMap
        (rewritingSet lt . toTree) $
        productions (reductionGrammars !! n)
    rewritingSet lt rt @ (App _ _)
        | lt `rew` larg = [rt]
        | larg `rew` lt = [foldl App s $ init ags ++ [lt]]
        | otherwise = map (App $ foldl App s (init ags)) $ meshSet lt larg
        where
            s = starting rt
            ags = args rt
            larg = last ags

    -- | Normal-order reduction grammar.
    data Grammar = Grammar
         { r :: Int
         , productions :: [Production]
         }
   
    printG :: Show a => [a] -> ShowS
    printG [] = (++) ""
    printG (p : ps) = foldl (.) (shows p) $
        map (\x -> (++) " | " . shows x) ps

    instance Show Grammar where
        showsPrec _ (Grammar n ps) = 
            (:) 'R' . shows n . (++) " = " . printG ps
  
    -- | Returns the size of the given grammar.
    size :: Grammar -> Int
    size = length . productions
   
    -- | Returns K-Expansions of the given production.
    kExpansions :: Production -> [Production]
    kExpansions p = case p of
        (ProdK as) -> kExpansions' K as
        (ProdS as) -> kExpansions' S as
        where
            kExpansions' _ [] = []
            kExpansions' h gs @ (x:xs) = ProdK (h : C : gs)
                : kExpansions' (App h x) xs
   
    -- | Returns S-Expansions of the given production.
    sExpansions :: Production -> [Production]
    sExpansions p = case p of
        (ProdK as) -> sExpansions' K as
        (ProdS as) -> sExpansions' S as
        where
            sExpansions' _ [] = []
            sExpansions' _ [_] = []
            sExpansions' h (x:y:xs) =
                map (\(App lt rt) -> ProdS (h : lt : rt : xs))
                    (rewritingSet x y) ++ sExpansions' (App h x) (y : xs)
    
    -- | R_0 reduction grammar.
    r0 :: Grammar
    r0 = Grammar { r = 0, productions = ps }
        where
            ps = [ProdS [], ProdK [], ProdS [R 0],
                  ProdK [R 0], ProdS [R 0, R 0]]

    shortProductions :: Int -> [Production]
    shortProductions n = [ProdS [R n], ProdK [R n]] ++
        [ProdS [R i, R $ n-i] | i <- [0..n]]

    -- | Given R_n constructs the grammar R_{n+1}.
    generate :: Grammar -> Grammar
    generate g = Grammar { r = r g + 1,
                           productions =
                                shortProductions (r g + 1) ++
                                [ProdK [R $ r g, C]] ++
                                concatMap kExpansions (productions g) ++
                                concatMap sExpansions (productions g)}
   
    -- | An infinite sequence of reduction grammars.
    reductionGrammars :: [Grammar]
    reductionGrammars = r0 : next reductionGrammars
        where
            next (g : gs) = generate g : next gs
