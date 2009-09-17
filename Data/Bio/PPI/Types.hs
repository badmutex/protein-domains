module Data.Bio.PPI.Types where

data Protein = Protein String deriving (Eq, Read, Show)
data Domain  = Domain  String deriving (Eq, Read, Show)


class ProteinDomain a where
    protein :: a -> Protein
    aliases :: a -> [Protein]
    domains :: a -> [Domain]


data Species = MkSpecies String deriving (Eq, Read, Show)



