-- | Parser for Pfam domain files. The format for one protein is
--
-- > >protein alias1 alias2 ...
-- > domain
-- > domain
-- > ...
-- > space\n
-- >
--
-- For example
--
-- > >P15711 104K_THEPA
-- > FAINT 
-- >  


module Data.Bio.PPI.PfamParser where

import Data.Bio.PPI.Types
import qualified Data.Bio.PPI.ProteinDomains as PD

import Text.Parsec
import Text.Parsec.ByteString.Lazy
import Control.Monad
import qualified Data.ByteString.Lazy as BS

validName :: Parser String
validName = many (alphaNum <|> char '_' <|> char '-')

domain :: Parser Domain
domain = do
  d <- validName ; space ; newline
  return Domain {domainName = CommonName d}

sectionEnd :: Parser Char
sectionEnd = char ' ' >> newline

domains :: Parser [Domain]
domains = domain `manyTill` sectionEnd


proteinAndDomains :: Parser PD.ProteinDomain
proteinAndDomains = do
  char '>'
  prot <- validName
  anyChar `manyTill` newline -- skip the aliases
  ds   <- domains
  return PD.ProteinDomain { PD.protein = Protein { proteinPfamID = PfamID prot }
                          , PD.domains = ds }

emptyLine :: Parser Char
emptyLine = spaces >> newline


translatePfamToProteinDomain :: Parser [PD.ProteinDomain]
translatePfamToProteinDomain = spaces >> proteinAndDomains `manyTill` eof


-- | accepts a generator and parses the contents
--
-- > translatePfam getContents
--
-- or
--
-- > translatePfam (readFile "foo.pfam")
translatePfam :: IO BS.ByteString -> IO (Either ParseError [PD.ProteinDomain])
translatePfam = fmap (parse translatePfamToProteinDomain "")


translatePfam' :: IO BS.ByteString -> IO ()
translatePfam' gen = let fixer (Left e)    = error $ "[translatePfam'] " ++ show e
                         fixer (Right pds) = foldM_ (\_ pd -> print pd) () pds
                     in translatePfam gen >>= fixer
