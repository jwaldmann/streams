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
     <> progDesc "example: find-fst -s 4 -w 1 -c 10000 -f Thue -t 'Snd Thue'"
     <> progDesc "stream syntax: data Stream = Fib | Thue | Morse | Waltz | Pdbl | Snd Stream | Thrd Stream"
   )

data Stream = Fib | Thue | Morse | Waltz | Pdbl
            | Snd Stream | Thrd Stream
  deriving (Eq, Ord, Read, Show)

data Config =
  Config { states :: Int -- ^ of FST
         , width :: Int -- ^ of output words of FST transitions
         , check :: Int -- ^ for input word of that length
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
    ( long "width" <> short 'w'
      <> metavar "INT"
      <> help "max. length of output word of a FTS transition"
    )
  <*> option auto
    ( long "check" <> short 'c' <> metavar "INT"
      <> help "check for prefix of input of that length"
    )
  <*> option auto
    ( long "from" <> short 'f' <> metavar "STREAM"
      <> help "expression for input stream"
    )
  <*> option auto
    ( long "to" <> short 't' <> metavar "STREAM"
      <> help "expression for output stream"
    )
