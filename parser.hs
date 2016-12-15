import Control.Monad.State
import Data.Ratio
import Text.Regex (splitRegex, mkRegex)
import Text.Read

type Point = (Rational, Rational)
type LineSeg = (Point, Point)
type Polygon = [Point]
type Silhouette = [Polygon]
type Skeleton = [LineSeg]
type Problem = (Silhouette, Skeleton)
type Parse a = State [String] a

tokenize :: String -> [String]
tokenize = splitRegex (mkRegex "[[:space:],]")

repeatP :: Integer -> Parse a -> Parse [a]
repeatP 0 _ = return []
repeatP n p = p >>= (\a -> fmap (a:) (repeatP (n-1) p))

parseNum :: Parse Integer
parseNum = state $ \(x:xs) -> (read x :: Integer ,xs)

parseRational :: Parse Rational
parseRational = state runParseRational

runParseRational :: [String] -> (Rational,[String])
runParseRational (x:xs) = 
  let r = if (elem '/' x) then (read n) % (read $ head d) else (read x) % 1
      (n:d) = split x
      split = splitRegex (mkRegex "/")
  in (r,xs)

parsePoint :: Parse Point
parsePoint = fmap (\[x,y] -> (x,y)) (repeatP 2 parseRational)

parseLineSeg :: Parse LineSeg
parseLineSeg = fmap (\[x,y] -> (x,y)) (repeatP 2 parsePoint)

parsePolygon :: Parse Polygon
parsePolygon = parseNum >>= (\n -> repeatP n parsePoint)

parseSilhouette :: Parse Silhouette
parseSilhouette = parseNum >>= (\n -> repeatP n parsePolygon)

parseSkeleton :: Parse Skeleton
parseSkeleton = parseNum >>= (\n -> repeatP n parseLineSeg)

parseProblem :: Parse Problem
parseProblem = do
  sil <- parseSilhouette
  skel <- parseSkeleton
  return (sil,skel)

--From the original problem spec here: http://icfpc2016.blogspot.jp/
exampleProb =
  "1 \
\4 \
\0,0 \
\1,0 \
\1/2,1/2 \
\0,1/2 \
\5 \
\0,0 1,0 \
\1,0 1/2,1/2 \
\1/2,1/2 0,1/2 \
\0,1/2 0,0 \
\0,0 1/2,1/2"


main :: IO ()
main = print $ show $ fst $ runState parseProblem (tokenize exampleProb)
