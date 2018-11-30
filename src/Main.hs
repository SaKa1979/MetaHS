{-|
Module      : Test
Description : Main module for compilation.
License     : None
Maintainer  : hhrf.vos@studie.ou.nl
Stability   : experimental

Main module for compilation.
-}
module Main
    where

import System.Environment (getArgs)
import Test

-- | Main function for profiling purposes.
main :: IO ()
main = do
    args <- getArgs
    _ <- if null args
    then error "An argument indicating which test to run is required!"
    else case head args of
        "0" -> do
            _ <- runAll
            return ("", [])
        "1" -> timedTest settings_ampersand_3_0_2
        "2" -> timedTest settings_helium_1_8
        "3" -> timedTest settings_ideas_1_2
        "4" -> timedTest settings_ideas_1_6
        "5" -> timedTest settings_pandoc_1_13_1
        "6" -> timedTest settings_parsec_3_1_7
        "7" -> timedTest settings_QuickCheck_2_7_6
        "8" -> timedTest settings_uulib_0_9_16
        "9" -> timedTest settings_metaHS
        "10" -> timedTest settings_example
        _ -> error "Unkown argument!"
    return ()
