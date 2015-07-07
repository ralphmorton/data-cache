
module Data.Cache(
	Data.Cache.Types.Cache,
	createCache,
	fetch
) where 

import Data.Cache.Types

import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Lens
import Control.Monad
import Data.List (sortBy)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)

createCache :: Maybe Int -> Maybe Int -> (k -> IO v) -> IO (Cache k v)
createCache ttlSecs maxItemCount lu = do 
	ix <- atomically $ newTVar M.empty
	return $ C lu ix ttlSecs maxItemCount
	
fetch :: Ord k => k -> Cache k v -> IO v
fetch k c = do 
	items <- readTVarIO (c^.items)
	tme <- time
	let mval = view (at k) items >>= getVal c tme
	maybe (fetchIO k c) (return) mval
	
fetchIO :: Ord k => k -> Cache k v -> IO v
fetchIO k c = do 
	let lu = c^.lup
	v <- lu k
	tme <- time
	atomically $ modifyTVar (c^.items) $ cacheInsert c k (I v tme)
	return v

cacheInsert :: Ord k => Cache k v -> k -> Item v -> CMap k v -> CMap k v
cacheInsert (C _ _ ttl max) k i m = cachePurge (i^.created) $ set (at k) (Just i) m
	where 
	cachePurge tme m
		| M.size m > bmax max = strip tme m
		| otherwise = m
	strip tme = 
		M.fromList . 
		take (bmax max) . 
		reverse . 
		sortBy (\x y -> compare (x ^. _2.created) (y ^. _2.created)) . 
		filter (\(_,i) -> (i^.created) + bmax ttl > tme) .
		M.toList
		
getVal :: Cache k v -> Int -> Item v -> Maybe v
getVal (C _ _ ttl max) tme i 
	| (i^.created) + bmax ttl < tme = Nothing
	| otherwise = Just $ i ^. val

time = fmap round $ getPOSIXTime

bmax :: Bounded a => Maybe a -> a
bmax = fromMaybe maxBound
