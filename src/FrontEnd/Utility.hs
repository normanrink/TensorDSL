
module Utility ( showUnquoted ) where

showUnquoted :: (Show a) => a -> String
showUnquoted s = tail . init $ show s
