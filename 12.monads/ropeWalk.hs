-- birds can land on both sides of the
-- pole and it only becomes unballanced
-- if abs(birdsLeft - birdsRight) > 3
-- pole -> Nothing


type Birds = Int
type Pole  = Birds, Birds

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left,right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left,right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise                    = Nothing

-- helper --
x (-:) f = f x

ghci> (0,0) -: landLeft 1 -: landRight 4 -: landLeft (-1) -: landRight (-2)
(0,2)
-- this does not catch the intermediate invalid state
-- of the pole and shows a valid state in the end.
-- we would want to see the invalid state propagating
-- to the end of the sequence. lets try using bind operator
-- of the Maybe monad now:

ghci> return (0,0) >>= landLeft 1 >>= landRight 4
  >>= landLeft (-1) >>= landRight (-2)
-- Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing

ghci> return (0,0) >>= landLeft 1 >>= banana >>= landRight 1
-- Nothing
-- or --
ghci> return (0,0) >>= landLeft 1 >> Nothing >>= landRight 1
-- Nothing

-- now usin case --
routine :: Maybe Pole
routine = case landLeft 1 (0,0) of
    Nothing -> Nothing
    Just pole1 -> case landRight 4 pole1 of
        Nothing -> Nothing
        Just pole2 -> case landLeft 2 pole2 of
            Nothing -> Nothing
            Just pole3 -> landLeft 1 pole3  
