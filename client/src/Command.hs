{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}

module Command (Cmd, cmd, execCmd) where
  import Control.Applicative

  newtype Cmd a = Cmd [IO a]
    deriving (Functor)

  instance Applicative Cmd where
    pure    = cmd . return
    (Cmd f) <*> (Cmd xs) = Cmd (zipWith (<*>) f xs)

  instance Monoid (Cmd a) where
    mempty = Cmd []
    mappend (Cmd xs) (Cmd ys) = Cmd (xs ++ ys)

  cmd :: IO a -> Cmd a
  cmd = Cmd . return

  execCmd :: (model -> msg -> (model, Cmd msg)) -> model -> Cmd msg -> IO model --TODO fold it
  execCmd _ m (Cmd [])     = return m
  -- FIXME this is highly recursive, a recursive cmd will break the app
  execCmd u m (Cmd (c:cs)) = do
    msg <- c
    let (m',cs') = u m msg
    execCmd u m' (Cmd cs `mappend` cs')
