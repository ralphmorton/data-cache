{-# LANGUAGE TemplateHaskell #-}

module Data.Cache.Types where 

import Control.Concurrent.STM.TVar
import Control.Lens.TH
import qualified Data.Map as M

data Item v = I {
	_val :: v,
	_created :: Int
}

makeLenses ''Item

type CMap k v = M.Map k (Item v)

data Cache k v = C {
	_lup :: k -> IO v,
	_items :: TVar (CMap k v),
	_ttl :: Maybe Int,
	_maxSize :: Maybe Int
}

makeLenses ''Cache