{-# LANGUAGE OverloadedStrings #-}

module Codec.MBox (MBox, Message(..), parse) where

import Data.Bool (bool)
import Data.Text (Text)
import qualified Data.Text as T

type MBox = [Message]

data Message = Message
    { msgSender :: Text
    , msgTime :: Text
    , msgExtra :: Text
    , msgContents :: Text
    } deriving (Eq, Show)

parse :: Text -> MBox
parse = snd . parseLines . T.lines

parseLines :: [Text] -> ([Text], MBox)
parseLines [] = ([], [])
parseLines (l : ls) = case T.stripPrefix "From " l of
    Just l' -> ([], message l' (T.unlines bdy') : msgs)
    Nothing -> (bool l (T.drop 1 l) (T.isPrefixOf ">From " l) : bdy, msgs)
  where
    ~(bdy, msgs) = parseLines ls
    bdy' = fst <$> zip bdy (drop 1 bdy)

message :: Text -> Text -> Message
message l bdy = Message
    { msgSender = sndr
    , msgTime = time
    , msgExtra = extra
    , msgContents = T.replace "\n" "\r\n" $ bdy
    }
  where
    (sndr, l') = T.breakOn " " l
    (time, l'') = T.splitAt 24 $ T.drop 1 l'
    extra = T.drop 1 l''
