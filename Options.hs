module Options

( main_with
, Config (..)
, Stream (..)
)

where

import Options.Applicative

main_with = execParser opts

opts = info (helper <*> config)
   ( fullDesc
     <> header "find FST that transforms one HD0L stream to another"
     <> progDesc (unlines 
       [ "example: find-fst -s 4 -w 1 -l 2 -c 10000 -f Thue -t 'Snd Thue'"
       , "stream syntax: data Stream = Fib | Thue | Morse | Waltz | PD | Sierp | Kolak | PF | Snd Stream | Thrd Stream | Delta Stream | Inv Stream"
       ] )
   )

data Stream = Fib | Thue | Morse | Waltz | PD | Sierp | Kolak | PF | Snd Stream | Thrd Stream | Delta Stream | Inv Stream
  deriving (Eq, Ord, Read, Show)

data Config =
  Config { states :: Int -- ^ of FST
         , maxwidth :: Int
         , minwidth :: Int
         , check :: Int -- ^ for input word of that length
         , limit :: Maybe Int -- ^ limit number of outputs
         , from :: Stream
         , to :: Stream
         }

config :: Parser Config
config = Config
  <$> option auto
    ( long "states" <> short 's'
      <> metavar "INT"
      <> help "number of states of the FST"
    )
  <*> option auto
    ( long "maxwidth" <> short 'w'
      <> metavar "INT"
      <> help "max. length of output word of a FTS transition"
    )
  <*> option auto
    ( long "minwidth" <> short 'i'
      <> showDefault
      <> metavar "INT"
      <> help "min. length of output word of a FTS transition"
      <> value 0
    )
  <*> option auto
    ( long "check" <> short 'c' <> metavar "INT"
      <> help "check for prefix of input of that length"
    )
  <*> option (Just <$> auto)
    ( value Nothing
      <> long "limit" <> short 'l' <> metavar "INT"
      <> help "limit number of FSTs that are output"
    )
  <*> option auto
    ( long "from" <> short 'f' <> metavar "STREAM"
      <> help "expression for input stream"
    )
  <*> option auto
    ( long "to" <> short 't' <> metavar "STREAM"
      <> help "expression for output stream"
    )
