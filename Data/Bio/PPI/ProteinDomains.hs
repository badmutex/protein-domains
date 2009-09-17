module Data.Bio.PPI.ProteinDomains where

import qualified Data.Bio.PPI.Types as PD (ProteinDomain(..))
import Data.Bio.PPI.Types hiding (ProteinDomain)

data ProteinDomain = MkProteinDomain {
      protein :: Protein
    , aliases :: [Protein]
    , domains :: [Domain]
    } deriving (Eq, Read, Show)




