module Main where

import Data.Bio.PPI.Types
import Data.Bio.PPI.ProteinDomains
import Data.Bio.PPI.ProteinSpecies
import Data.Bio.PPI.PfamParser

import Text.ParserCombinators.Parsec
import System.Environment

pfamData = "test/test.pfam"

test = translatePfam' (readFile pfamData)

main = translatePfam' getContents
  
  