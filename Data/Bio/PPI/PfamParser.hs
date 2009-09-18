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
import qualified Data.ByteString.Lazy.Char8 as BS
import System.IO.Unsafe
import Data.List

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
translatePfam' gen = translatePfam gen >>= fixer

fixer :: Either ParseError [PD.ProteinDomain] -> IO ()
fixer = undefined
-- fixer (Right pds) = foldl1' (\_ pd -> let !pd' = pd in print pd') pds
-- fixer (Left e)    = error $ "[translatePfam'] " ++ show e
--fixer (Right pds) = return $ foldl' (\_ pd -> unsafePerformIO $ print pd) undefined pds
  


work :: PD.ProteinDomain -> String -> IO PD.ProteinDomain
work pd " " = print pd >> return pd
work _ ('>':p) = return $ PD.ProteinDomain { PD.protein = Protein {proteinPfamID = PfamID p}
                                           , PD.domains = []}
          where (pfam:rest) = words p
work pd d = return pd { PD.domains = Domain { domainName = CommonName d }
                                     : PD.domains pd }

translatePfam'' :: IO String -> IO ()
translatePfam'' gen = gen >>= foldM_ work undefined . tail . lines