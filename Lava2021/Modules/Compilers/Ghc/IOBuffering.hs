module Compilers.Ghc.IOBuffering where

import GHC.IO.Handle (hSetBuffering, BufferMode(..))
import GHC.IO.Handle.FD (stdout)

noBuffering :: IO ()
noBuffering = hSetBuffering stdout NoBuffering
