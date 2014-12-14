{-# Language FlexibleContexts #-}

import Control.Eff
import Control.Eff.Exception
import Data.OpenUnion

newtype Exc e v = Exc e

throwError :: (Member (Exc e) r) => e -> Eff r a
throwError e = send req (const $ Exc e)