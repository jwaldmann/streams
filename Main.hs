-- | check experimentally whether one stream
-- is the FT image of another

{-# language LambdaCase #-}
{-# language GeneralizedNewtypeDeriving #-}

import Options

import Data.Char (ord, chr)
import qualified Data.Map.Strict as M
import Control.Monad ( guard, forM_ )

main = main_with >>= \ conf -> do
  putStrLn $ unlines
    [ "input  : " ++ show (take 20 $ eval $ from conf)
    , "output : " ++ show (take 20 $ eval $ to   conf)
    ]  
  mapM print $ fsts (states conf) (width conf)
      (take (check conf) $ eval $ from conf)
      (eval $ to conf)

-- * search

-- | this finds the morphism 0 -> 011, 1 -> 91, 2 -> 0
-- from morse [0,1,2,0,2,1,0,1,2,1,0,2,0,1,2,0,2,1,0,2] ...
-- to   thue  [0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,0,0,1] ...
-- (note: argument 1 : one state = morphism )
test1 = fsts 1 3 (take 1000 morse) thue

-- | all fts with q states and max image length l
-- that map stream s to stream t.
-- search stops successfully if s is consumed.
-- (call this function with some finite prefix s)
fsts :: Int -> Int -> [Int] -> [Int] -> [ FST Q Int ]
fsts q l s t =  
  let work :: M.Map (Q,Int) ([Int],Q)
           -> Q -> [Int] -> [Int]
           -> [ FST Q Int ]
      work m z [] _ = do
        let ft = FST 0 m
        -- need to check here that ft s is productive.
        -- following approximation is not very good
        -- (and can be fooled)
        let n = truncate $ sqrt $ realToFrac $ length s
        guard $ apply ft s `islongerthan` n
        return ft
      work m z (x:xs) ys = case M.lookup (z,x) m of
        Just (w,z') -> do
          let (pre,post) = splitAt (length w) ys
          guard $ pre == w
          work m z' xs post
        Nothing -> do
          z' <- take q [ 0 .. ]
          k <- [ 0 .. l ]
          let (pre,post) = splitAt k ys
          let m' = M.insert (z,x) (pre,z') m
          work m' z' xs post
  in  work M.empty (Q 0) s t

islongerthan xs k = not $ null $ drop k xs

-- * streams

streamfix :: Int -> (Int-> [Int]) -> [Int]
streamfix x f = let s = x : tail ( s >>= f) in s

fib   = streamfix 0 $ \ case 0 -> [0,1]  ; 1 -> [0] 
thue  = streamfix 0 $ \ case 0 -> [0,1]  ; 1 -> [1,0]
morse = streamfix 0 $ \ case 0 -> [0,1,2]; 1 -> [0,2]; 2 -> [1]
pdbl  = streamfix 0 $ \ case 0 -> [0,1] ; 1 -> [0,0]
pdbl3 = third pdbl
waltz = streamfix 0 $ \ case 0 -> [0,0,1]; 1 -> [1,1,0]

eval = \ case
  Fib -> fib
  Thue -> thue
  Morse -> morse
  Pdbl -> pdbl
  Waltz -> waltz
  Snd x -> second $ eval x
  Thrd x -> third $ eval x

-- * finite transducers

-- | substream: every second letter
second (x:y:rest) = x: second rest

-- | substream: every third letter
third (x:y:z:rest) = x: third rest
  

data FST q a = FST q (M.Map (q,a) ([a],q))
  deriving Show

newtype Q = Q Int deriving (Eq, Ord, Enum, Num)

instance Show Q where show (Q q) = return $ chr (ord 'A' + q)

-- | example transducer
ft0 = FST 0 $ M.fromList $ do
  q <- [0,1] ; a <-[0,1]
  return ((q,a),([(q*a) `mod` 2], (q+a) `mod` 2))

-- | apply the finite transducer to a stream,
-- producing a stream
apply  (FST q0 m) s =
  let work q [] = []
      work q (x:xs) = let (w,q') = m M.! (q,x)
                      in w ++ work q' xs
  in work q0 s
     
