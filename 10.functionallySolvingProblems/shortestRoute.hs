data Section = Section {
    takeA :: Int,
    takeB :: Int,
    takeC :: Int
  } deriving (Show)

type RoadSystem = [Section]


data Path =
  Path String Int
  deriving (Show)

(+++) :: Path -> Path -> Path
(+++) (Path labels distance) (Path labels' distance') =
  Path (labels ++ labels') (distance + distance')

instance Eq Path where
  (==) (Path _ distance) (Path _ distance') =
    distance == distance'

instance Ord Path where
  (<) (Path _ distance) (Path _ distance') =
    distance < distance'
  (<=) path path' =
    (path == path') || (path < path')

-- * -- * -- *
--      |    |
-- * -- * -- *

shortestRoute :: RoadSystem -> (Path,Path)
shortestRoute [] = (Path "" 0, Path "" 0)
shortestRoute (section:nextSections) =
  let a = Path "a" (takeA section)
      b = Path "b" (takeB section)
      c = Path "c" (takeC section)
      (nextA,nextB) = shortestRoute nextSections
      fromA = min (a +++ nextA) (a +++ (c +++ nextB))
      fromB = min (b +++ nextB) (b +++ (c +++ nextA))
  in (fromA, fromB)


roads = [
    Section 50 10 30,
    Section 5 90 20,
    Section 40 2 25,
    Section 10 8 0
  ]

main = do
  let (pathA,pathB) = shortestRoute roads
  print $ min pathA pathB
