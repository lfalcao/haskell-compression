module Huffman
    (count, markov1, Tree, encode_huffman, decode_huffman)
  where
 
import Data.List (nub)
 
-- Marvok1 probability model...
 
count :: (Eq t) => [t] -> [(t,Int)]
count xs = map (\x -> (x, length $ filter (x ==) xs)) $ nub xs
 
markov1 :: (Eq t) => [t] -> [(t,Double)]
markov1 xs =
  let n = fromIntegral $ length xs
  in  map (\(x,c) -> (x, fromIntegral c / n)) $ count xs
 
 
-- Build a Huffman tree...
 
data Tree t = Leaf Double t | Branch Double (Tree t) (Tree t) deriving Show
 
prob :: Tree t -> Double
prob (Leaf   p _)   = p
prob (Branch p _ _) = p
 
get_tree :: [Tree t] -> (Tree t, [Tree t])
get_tree (t:ts) = work t [] ts where
  work x xs [] = (x,xs)
  work x xs (y:ys)
    | prob y < prob x = work y (x:xs) ys
    | otherwise       = work x (y:xs) ys
 
huffman_build :: [(t,Double)] -> Tree t
huffman_build = build . map (\(t,p) -> Leaf p t) where
  build [t] = t
  build ts =
    let (t0,ts0) = get_tree ts
        (t1,ts1) = get_tree ts0
    in  build $ Branch (prob t0 + prob t1) t0 t1 : ts1
 
 
-- Make codebook...
 
data Bit  = Zero | One deriving (Eq, Show)
type Bits = [Bit]
 
huffman_codebook :: Tree t -> [(t,Bits)]
huffman_codebook = work [] where
  work bs (Leaf _ x) = [(x,bs)]
  work bs (Branch _ t0 t1) = work (bs ++ [Zero]) t0 ++ work (bs ++ [One]) t1
 
 
-- Do the coding!
 
encode :: (Eq t) => [(t,Bits)] -> [t] -> Bits
encode cb = concatMap (\x -> maybe undefined id $ lookup x cb)
 
decode :: (Eq t) => Tree t -> Bits -> [t]
decode t = work t t where
  work _ (Leaf   _ x)        []  = [x]
  work t (Leaf   _ x)        bs  = x : work t t bs
  work t (Branch _ t0 t1) (b:bs)
    | b == Zero = work t t0 bs
    | otherwise = work t t1 bs
 
encode_huffman :: (Eq t) => [t] -> (Tree t, Bits)
encode_huffman xs =
  let t  = huffman_build $ markov1 xs
      bs = encode (huffman_codebook t) xs
  in (t,bs)
 
decode_huffman :: (Eq t) => Tree t -> Bits -> [t]
decode_huffman = decode