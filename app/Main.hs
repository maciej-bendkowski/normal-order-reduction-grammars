{-|
 - Module       : Main 
 - Description  : norg-gf executable module.
 - Copyright    : (c) Maciej Bendkowski, 2016
 - Maintainer   : maciej.bendkowski@tcs.uj.edu.pl
 - Stability    : experimental
 -}
module Main (main) where
    
    import System.IO
    import System.Exit
    import System.Console.GetOpt
    import System.Environment
    
    import Data.List (nub)
    import Algebra

    data Flag = Size String
              | Simplify (Maybe String)
              | Version
              | Help
                deriving (Eq)

    options :: [OptDescr Flag]
    options = [Option "n" ["size"] (ReqArg Size "n")
        "The norg-ogf to be computed.",

        Option "v" ["version"] (NoArg Version)
            "Prints the program version number.",
        
        Option "h?" ["help"] (NoArg Help)
            "Prints this help message."]

    getSize :: [Flag] -> Int
    getSize (Size n : _) = read n
    getSize (_:fs) = getSize fs
    getSize [] = -1

    usageHeader :: String
    usageHeader = "Usage: norg-gf [OPTIONS...]"

    versionHeader :: String
    versionHeader = "norg-gf version 2.0, (c) Maciej Bendkowski 2016"

    parse :: [String] -> IO ([Flag], [String])
    parse argv = case getOpt Permute options argv of
        (ops, nonops, [])
            | Help `elem` ops -> do
                putStrLn $ usageInfo usageHeader options
                exitSuccess
            | Version `elem` ops -> do
                putStrLn versionHeader
                exitSuccess
            | otherwise -> return (nub (concatMap mkset ops), fs)
                where
                    fs = if null nonops then ["-"] else nonops
                    mkset x = [x]
        (_, _, errs) -> do
            hPutStr stderr (concat errs ++ usageInfo usageHeader options)
            exitWith (ExitFailure 1)

    run :: [Flag] -> String -> IO ()
    run fgs _
        | n < 0 = do
            hPutStr stderr $ usageInfo usageHeader options
            exitWith (ExitFailure 1)
        | otherwise = putStrLn (toMathematica n "") 
        where
            n = getSize fgs

    main :: IO ()
    main = do
        (ops, fs) <- getArgs >>= parse
        mapM_ (run ops) fs
        exitSuccess
