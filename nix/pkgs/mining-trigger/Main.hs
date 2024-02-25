{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
  post "/submit" $ do
    _req <- jsonData :: ActionM Int
    return ()
