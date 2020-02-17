import qualified Data.Set as Set
import qualified Data.Text as T

digits = "0123456789_"

data MESON = MESONInteger Integer
           | MESONList [MESON]
           | MESONString String
           | MESONSet (Set.Set MESON)
           | MESONTuple [MESON]
           deriving (Eq, Ord, Show)

unescapeMap = [("%20"," "),
               ("%0A","\n"),
               ("%09","\t"),
               ("%0B","\v"),--vertical tab
               ("%25","%")] --note: this percent sign transformation must go last for obvious reasons

onText f (a, b) = f (T.pack a) $ T.pack b

unescape :: String -> String
unescape xs = T.unpack . (\xs -> foldl (flip (onText T.replace)) xs unescapeMap) . T.pack $ xs

parseMESONCode :: String -> MESON
parseMESONCode f = head $ foldl parseMESONToken (error "Bottom of stack!") (words f)

parseMESONToken :: [MESON] -> String -> [MESON]
parseMESONToken state str | all (`elem` digits) str           = (MESONInteger . read $ filter (/= '_') str) : state
parseMESONToken state ('"':str)                               = (MESONString $ unescape str) : state
parseMESONToken state ('(':xs) | (dropWhile (== ',') xs) == ")" = (MESONTuple $ take num state) : (drop num state)
                where num = succ . length $ takeWhile (== ',') xs
parseMESONToken ((MESONInteger num):state) "[]"               = (MESONList $ take numInt state) : (drop numInt state)
                where numInt = fromIntegral num
parseMESONToken ((MESONInteger num):state) "{}"               = (MESONSet . Set.fromList $ take numInt state) : (drop numInt state)
                where numInt = fromIntegral num


main = interact $ unlines . (map (show . parseMESONCode)) . lines