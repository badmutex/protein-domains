module Main where

import Data.Bio.PPI.Types
import Data.Bio.PPI.Instances
import qualified Data.Bio.PPI.ProteinDomains as PD


readProteinDomain :: (Read a, ProteinDomain a) => String -> a
readProteinDomain = read

readPD :: String -> PD.ProteinDomain
readPD = readProteinDomain

main = getContents >>= mapM_ (putStrLn . show . readPD) . lines