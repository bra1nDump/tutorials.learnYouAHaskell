type Zipper a = ([a], [a])

focus :: Num -> Zipper a -> Zipper a
focus 0 = id
focus steps
  | steps > 0 = (\(xs, y:ys) -> focus (steps - 1) (xs:y, ys))
  | otherwise = error "lol, cannot step back"
