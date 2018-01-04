-- laws --
return a >>= k  =  k a
m >>= return  =  m
m >>= (\x -> k x >>= h)  =  (m >>= k) >>= h

-- Furthermore, the Monad and Applicative
-- operations should relate as follows:

pure = return
(<*>) = ap

The above laws imply:

fmap f xs  =  xs >>= return . f

(>>) = (*>)
