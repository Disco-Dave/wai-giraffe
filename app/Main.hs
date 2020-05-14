{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Wai.Giraffe.Core
import           Network.Wai.Handler.Warp       ( run )

handler :: HttpHandler
handler = choose
  [ route "a" >==> choose [post >==> text "a post", get >==> text "a get"]
  , route "b" >==> choose [post >==> text "b post", get >==> text "b get"]
  ]

main :: IO ()
main = run 8888 (toApplication handler)
