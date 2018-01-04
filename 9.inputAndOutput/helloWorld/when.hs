import Control.Monad

-- when :: Applicative f => Bool -> f () -> f ()
-- if expression == True -> return IO action, else
-- return () == empty IO action

-- if something then do some I/O action else return ()

main = do
  c <- getChar
  when (c /= ' ') $ do
    putChar c
    main
