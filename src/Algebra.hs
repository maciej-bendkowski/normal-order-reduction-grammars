{-|
 - Module       : Algebra
 - Description  : Algebraic utilities for generating functions.
 - Copyright    : (c) Maciej Bendkowski, 2016
 - Maintainer   : maciej.bendkowski@tcs.uj.edu.pl
 - Stability    : experimental
 -}
module Algebra where
  
    import Data.List
    import qualified ReductionGrammar as R

    data GF = GF { constant :: Int
                 , monomial :: Int
                 , clgf :: Int
                 , reductions :: [(Int, Int)]
                 } deriving (Eq, Ord)

    showM :: Int -> ShowS
    showM 0 = (++) ""
    showM 1 = (++) "*z"
    showM n = (++) "*z^{" . shows n . (++) "}"

    showCL :: Int -> ShowS
    showCL 0 = (++) ""
    showCL 1 = (++) "*F[z]"
    showCL n = (++) "*(F[z])^{" . shows n . (++) "}"

    showR :: [(Int, Int)] -> ShowS
    showR [] = (++) ""
    showR ((r,1) : rs) = (++) "*Subscript[R," 
        . shows r . (++) "][z]"
        . showR rs
    showR ((r,p) : rs) = (++) "*(Subscript[R,"
        . shows r  . (++) "][z])^{" 
        . shows p . (++) "}"
        . showR rs

    showGF :: GF -> ShowS
    showGF gf
        | c == 0 = (++) "0"
        | k == 0 && cl == 0 && null rs = shows c
        | otherwise = shows c . showM k . showCL cl . showR rs
        where
            c = constant gf
            k = monomial gf
            cl = clgf gf
            rs = reductions gf

    instance Show GF where
        showsPrec _ = showGF

    defaultGF :: GF
    defaultGF = GF { constant = 1
                   , monomial = 0
                   , clgf = 0
                   , reductions = []
                   }
   
    merge :: [(Int, Int)] -> [(Int,Int)] -> [(Int,Int)]
    merge [] xs = xs
    merge xs [] = xs
    merge xs @ ((r,n) : xs')  ys @ ((r',n') : ys')
        | r == r' = (r, n + n') : merge xs' ys'
        | r < r' = (r,n) : merge xs' ys
        | otherwise = (r',n') : merge xs ys'

    termGF :: R.Tree -> GF
    termGF (R.R n) = defaultGF { reductions = [(n,1)] }
    termGF R.C = defaultGF { clgf = 1 }
    termGF (R.App l r) = GF { constant = cl * cr
                            , monomial = ml + mr + 1
                            , clgf = cll + clr
                            , reductions = merge rdsl rdsr
                            }
                       where
                          (tl, tr) = (termGF l, termGF r)
                          (cl, cr) = (constant tl, constant tr)
                          (ml, mr) = (monomial tl, monomial tr)
                          (rdsl, rdsr) = (reductions tl, reductions tr)
                          (cll, clr) = (clgf tl, clgf tr)
    termGF _ = defaultGF

    productionGF :: R.Production -> GF
    productionGF (R.ProdK []) = defaultGF
    productionGF (R.ProdK ts) = termGF p
        where
            p = foldl R.App R.K ts
    productionGF (R.ProdS []) = defaultGF
    productionGF (R.ProdS ts) = termGF p
        where
            p = foldl R.App R.S ts

    grammarGF :: R.Grammar -> [GF]
    grammarGF g = map productionGF (R.majorProds g)

    groupGFs :: [GF] -> [(GF, Int)]
    groupGFs = map (\xs @ (x:_) -> (x, length xs)) . group . sort

    simplify :: [GF] -> [GF]
    simplify gf = map (\(g,n) ->
        g {constant = constant g * n}) $ groupGFs gf

    filterRHS :: Int -> [GF] -> [GF]
    filterRHS n = filter (\gf -> n `notElem` map fst (reductions gf))

    mathematicaSum :: [GF] -> ShowS
    mathematicaSum [] = (++) ""
    mathematicaSum [g] = shows g
    mathematicaSum (g : gfs) = shows g . (++) " + " . mathematicaSum gfs

    toMathematica :: Int -> ShowS
    toMathematica 0 = (++) "(1 - 2*z - Sqrt[1 - 4*z - 4*z^2])/(2*z^2)"
    toMathematica n = (++) "(" . rhsNum . (++) ")/(" . rhsDen . (++) ")"
        where
            rhsNum = mathematicaSum rhs 
            rhs = filterRHS n $ simplify gf
            rhsDen = (++) "Sqrt[1 - 4*z - 4*z^2]"
            gf = grammarGF $ R.reductionGrammars !! n
