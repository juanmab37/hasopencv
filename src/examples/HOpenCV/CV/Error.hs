module HOpenCV.CV.Error
where

import Control.Monad.Error

data ErrorCV er a = Raise er | Return a deriving Show

instance (Error er) => Functor (ErrorCV er) where
        fmap = liftM

instance (Error er) => Applicative (ErrorCV er) where
        pure = return
        (<*>) = ap

instance (Error er) => Monad (ErrorCV er) where
        return x = Return x
        (Raise er) >>= f = Raise er
        (Return a) >>= f = f a
        fail msg = Raise (strMsg msg)

throwErrorCV :: er -> ErrorCV er a
throwErrorCV er = Raise er

type ErrCV a = ErrorCV String a

runErr :: ErrCV a -> a
runErr (Return a) = a

runErr_s :: ErrCV a -> String
runErr_s (Raise a) = a



{-
Para usar:
--http://www.randomhacks.net/2007/03/10/haskell-8-ways-to-report-errors/

:set -XFlexibleContexts

myDiv5 3 0 :: Either CustomError Float

--http://aprendehaskell.es/content/MasMonadas.html#errores-errores-errores

-}
