-- module Compression where
 
import List
import Maybe
import IO (hFlush, stdout)
 
chars = [' '..'~']   -- Becuase ' ' = 0x20 and '~' = 0x7F.
 
-- Run-length encoding
 
encode_RLE :: (Eq t) => [t] -> [(Int,t)]
encode_RLE = map (\xs -> (length xs, head xs)) . groupBy (==)
 
decode_RLE :: [(Int,t)] -> [t]
decode_RLE = concatMap (uncurry replicate)
 
 
-- Limpel-Ziv-Welch encoding
 
encode_LZW :: (Eq t) => [t] -> [t] -> [Int]
encode_LZW alphabet = work (map (:[]) alphabet) where
  chunk pred lst = last . takeWhile (pred . fst) . tail $ zip (inits lst) (tails lst)
  work table []  = []
  work table lst = fromJust (elemIndex tok table) : work (table ++ [tok ++ [head rst]]) rst
    where (tok, rst) = chunk (`elem` table) lst
 
decode_LZW :: [t] -> [Int] -> [t]
decode_LZW alphabet xs = concat output where
  output = map (table !!) xs
  table = map (:[]) alphabet ++ zipWith (++) output (map (take 1) (tail output))
 
main = do x <- take 20000 `fmap` readFile "/usr/share/dict/words"
          let l = length x `div` 80
              a = ['\0' .. '\255']
	      eq a b | a == b    = putChar '=' >> hFlush stdout
	             | otherwise = error "data error"
	      cmp = zipWith eq x . decode_LZW a . encode_LZW a $ x
              vl = map head $ unfoldr (\cm -> case cm of [] -> Nothing ; _ -> Just (splitAt l cm)) cmp
          sequence_ vl