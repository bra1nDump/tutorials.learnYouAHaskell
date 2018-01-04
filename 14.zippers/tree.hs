data Tree a =
  Node a (Tree a) (Tree a)
  | Empty
  deriving (Show)

sample = Node 4 (
    Node 40
      (Node 3 Empty Empty)
      Empty
  ) (
    Node 15
      (Node 5 Empty Empty)
      (Node 4 Empty Empty)
  )

data Direction = Left' | Right'
  deriving (Show)
type Path = [Direction]

modify :: Path -> (a -> a) -> Tree a -> Tree a
modify [] f (Node value l r) = Node (f value) l r
modify (Left':xs) f (Node value l r) = Node value (modify xs f l) r
modify (Right':xs) f (Node value l r) = Node value l (modify xs f r)

type TreeZipper a = (Tree a, Path)

focus :: Path -> TreeZipper a -> TreeZipper a
focus [] zipper = zipper
focus (Left':xs) (Node _ l r, ys) = focus xs (l, Left':ys)
focus (Right':xs) (Node _ l r, ys) = focus xs (r, Right':ys)

data BreadCrumb a = BreadCrumb Direction a (Tree a)
  deriving (Show)

type TreeZipper' a = (Tree a, [BreadCrumb a])

focus' :: Path -> TreeZipper' a -> TreeZipper' a
focus' [] zipper = zipper
focus' (Left':xs) (Node value l r, ys) =
  focus' xs (l, BreadCrumb Left' value r:ys)
focus' (Right':xs) (Node value l r, ys) =
  focus' xs (r, BreadCrumb Right' value l:ys)

goUp :: TreeZipper' a -> TreeZipper' a
goUp (focusBranch, BreadCrumb Left' value right:xs) =
  (Node value focusBranch right, xs)
goUp (focusBranch, BreadCrumb Right' value left:xs) =
  (Node value left focusBranch, xs)

focusedSample = focus' [Left', Left'] (sample,[])

-- helpers --
(-:) x f = f x
