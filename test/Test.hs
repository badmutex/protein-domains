module Main where

import Data.Bio.PPI.Types
import Data.Bio.PPI.ProteinDomains
import Data.Bio.PPI.ProteinSpecies
import Data.Bio.PPI.PfamParser

import Text.Parsec
import qualified Data.ByteString.Lazy as BS
import System.Environment

pfamData = "test/test.pfam"

test = translatePfam' (BS.readFile pfamData)

main = getArgs >>= translatePfam' . BS.readFile . head
