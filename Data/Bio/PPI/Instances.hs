module Data.Bio.PPI.Instances where

import qualified Data.Bio.PPI.Types as C (ProteinDomain(..))
import qualified Data.Bio.PPI.ProteinDomains as PD (ProteinDomain(..))

instance C.ProteinDomain PD.ProteinDomain where
    protein = PD.protein
    aliases = PD.aliases
    domains = PD.domains