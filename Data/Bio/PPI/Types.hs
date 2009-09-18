{-# LANGUAGE
  MultiParamTypeClasses
  , TypeSynonymInstances
  #-}

module Data.Bio.PPI.Types where

import Data.From

class Relation a b c where
    source :: c -> a
    others :: c -> [b]


class Name a where
    name :: a -> CommonName

class Pfam a where
    pfamID :: a -> PfamID



newtype PfamID     = PfamID     String deriving (Eq, Read, Show)
newtype CommonName = CommonName String deriving (Eq, Read, Show)

instance From PfamID     String where from (PfamID s)     = s
instance From CommonName String where from (CommonName s) = s


data Protein = Protein {
      proteinPfamID :: PfamID
    } deriving (Eq, Read, Show)

instance Pfam Protein where pfamID = proteinPfamID


data Domain = Domain {
      domainName   :: CommonName
--    , domainPfamID :: PfamID
    } deriving (Eq, Read, Show)

instance Name Domain where name   = domainName
-- instance Pfam Domain where pfamID = domainPfamID


type Genus   = String
type Species = String

data Organism = Organism {
      genus   :: Genus
    , species :: Species
    } deriving (Eq, Read, Show)