{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Bio.PPI.ProteinSpecies where

import Data.Bio.PPI.Types


data ProteinSpecies = ProteinSpecies {
      protein :: Protein
    , organism :: [Organism]
    } deriving (Eq, Read, Show)


instance Relation Protein Organism ProteinSpecies where
    source = protein
    others = organism
