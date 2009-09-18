{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Bio.PPI.ProteinDomains where

import Data.Bio.PPI.Types


data ProteinDomain = ProteinDomain {
      protein :: Protein
    , domains :: [Domain]
    } deriving (Eq, Read, Show)


instance Relation Protein Domain ProteinDomain where
    source = protein
    others = domains
