{-# LANGUAGE PackageImports #-}
import "cabal2nix" Application (develMain)
import Prelude (IO)

main :: IO ()
main = develMain
