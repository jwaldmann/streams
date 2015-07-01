-- | check experimentally whether one stream
-- is the FT image of another

{-# language LambdaCase #-}

import qualified Data.Map.Strict as M
import Control.Monad ( guard )

-- * search

-- | all fts with q states and max image length l
-- that map stream s to stream t.
-- search stops successfully if s is consumed.
-- (call this function with some finite prefix s)
fts q l s t =  
  let work m z [] _ = do
        let ft = FT 1 m
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
          z' <- [ 1 .. q ]
          k <- [ 0 .. l ]
          let (pre,post) = splitAt k ys
          let m' = M.insert (z,x) (pre,z') m
          work m' z' xs post
  in  work M.empty 1 s t

islongerthan xs k = not $ null $ drop k xs

-- * streams

streamfix x f = let s = x : tail ( s >>= f) in s

fib = streamfix 0 $ \ case 0 -> [0,1] ; 1 -> [0] 
thue = streamfix 0 $ \ case 0 -> [0,1] ; 1 -> [1,0]
morse = streamfix 0 $ \ case 0 -> [0,1,2]; 1 -> [0,2]; 2 -> [1]

-- * finite transducers

third (x:y:z:rest) = x: third rest
  

data FT q a = FT q (M.Map (q,a) ([a],q))
  deriving Show

-- | example transducer
ft0 = FT 0 $ M.fromList $ do
  q <- [0,1] ; a <-[0,1]
  return ((q,a),([(q*a) `mod` 2], (q+a) `mod` 2))

-- | apply the finite transducer to a stream,
-- producing a stream
apply  (FT q0 m) s =
  let work q [] = []
      work q (x:xs) = let (w,q') = m M.! (q,x)
                      in w ++ work q' xs
  in work q0 s
     
