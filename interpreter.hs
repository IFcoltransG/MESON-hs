import qualified Data.Set as Set
import qualified Data.Text as T

digits = "0123456789_"

data MESON = MESONInteger Integer
           | MESONList [MESON]
           | MESONString String
           | MESONSet (Set.Set MESON)
           | MESONTuple [MESON]
           deriving (Eq, Ord)

unescapeMap = [("%20"," "),
               ("%0A","\n"),
               ("%09","\t"),
               ("%0B","\v"),--vertical tab
               ("%25","%")] --note: this percent sign transformation must go last for obvious reasons

onText f (a, b) = f (T.pack a) $ T.pack b

unescape :: String -> String
unescape xs = T.unpack . (\xs -> foldl (flip (onText T.replace)) xs unescapeMap) . T.pack $ xs

--parseMESONCode :: String -> MESON
--parseMESONCode f = head $ foldr parseMESONToken [] (words f)

--parseMESONToken :: String -> [MESON] -> [MESON]
--parseMESONToken str state | all (`elem` digits) str = (MESONInteger . read $ filter (/= '_') str) ++ state
--parseMESONToken ('"':str) state = (unescape str) ++ state


main = do
     putStrLn "Not implemented"