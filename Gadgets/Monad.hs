module Gadgets.Monad where

import           Control.Applicative (liftA2)
import           Control.Monad.Trans.Writer 
  (WriterT, Writer, censor, runWriter, runWriterT)

-- | Clear the log of a @WriterT@.
-- 
clear :: (Monoid w, Monad m) => WriterT w m a -> WriterT w m a
clear = censor $ const mempty

-- | Extract the result from a writer computation.
--
evalWriter :: Monoid w => Writer w a -> a
evalWriter = fst . runWriter

-- | Extract the result from a writer computation.
--
evalWriterT :: (Monad m, Monoid w) => WriterT w m a -> m a
evalWriterT = fmap fst . runWriterT

-- | For monads, @ void_ = return () @.
-- 
void_ :: Applicative f => f ()
void_ = pure ()
