import Control.Monad.State
import Data.Ratio
import Text.Regex (splitRegex, mkRegex)
import Text.Read

type Parse a = State [String] a

tokenize :: String -> [String]
tokenize = splitRegex (mkRegex "[[:space:],]")

repeatP :: Integer -> Parse a -> Parse [a]
repeatP 0 _ = return []
repeatP n p = p >>= (\a -> fmap (a:) (repeatP (n-1) p))

parseNum :: Parse Integer
parseNum = state $ \(x:xs) -> (read x :: Integer ,xs)

  
main :: IO ()
main = print $ show $ runState parseNum (tokenize "0,0 1,0")
