{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MagicHash                  #-}
module Basement.Alg.PrimArray
    ( Indexable, index
    , findIndexElem
    , revFindIndexElem
    , findIndexPredicate
    , revFindIndexPredicate
    , foldl
    , foldr
    , foldl1
    , all
    , any
    , filter
    ) where

import           GHC.Types
import           GHC.Prim
import           Basement.Alg.Class
import           Basement.Compat.Base
import           Basement.Numerical.Additive
import           Basement.Numerical.Multiplicative
import           Basement.Types.OffsetSize
import           Basement.PrimType
import           Basement.Monad

findIndexElem :: (Indexable container ty, Eq ty) => ty -> container -> Offset ty -> Offset ty -> Offset ty
findIndexElem ty ba startIndex endIndex = loop startIndex
  where
    loop !i
        | i < endIndex && t /= ty = loop (i+1)
        | otherwise               = i
      where t = index ba i
{-# INLINE findIndexElem #-}

revFindIndexElem :: (Indexable container ty, Eq ty) => ty -> container -> Offset ty -> Offset ty -> Offset ty
revFindIndexElem ty ba startIndex endIndex
    | endIndex > startIndex = loop (endIndex `offsetMinusE` 1)
    | otherwise             = endIndex
  where
    loop !i
        | t == ty        = i
        | i > startIndex = loop (i `offsetMinusE` 1)
        | otherwise      = endIndex
      where t = index ba i
{-# INLINE revFindIndexElem #-}

findIndexPredicate :: Indexable container ty => (ty -> Bool) -> container -> Offset ty -> Offset ty -> Offset ty
findIndexPredicate predicate ba !startIndex !endIndex = loop startIndex
  where
    loop !i
        | i < endIndex && not found = loop (i+1)
        | otherwise                 = i
      where found = predicate (index ba i)
{-# INLINE findIndexPredicate #-}

revFindIndexPredicate :: Indexable container ty => (ty -> Bool) -> container -> Offset ty -> Offset ty -> Offset ty
revFindIndexPredicate predicate ba startIndex endIndex
    | endIndex > startIndex = loop (endIndex `offsetMinusE` 1)
    | otherwise             = endIndex
  where
    loop !i
        | found          = i
        | i > startIndex = loop (i `offsetMinusE` 1)
        | otherwise      = endIndex
      where found = predicate (index ba i)
{-# INLINE revFindIndexPredicate #-}

foldl :: Indexable container ty => (a -> ty -> a) -> a -> container -> Offset ty -> Offset ty -> a
foldl f !initialAcc ba !startIndex !endIndex = loop startIndex initialAcc
  where
    loop !i !acc
        | i == endIndex = acc
        | otherwise     = loop (i+1) (f acc (index ba i))
{-# INLINE foldl #-}

foldr :: Indexable container ty => (ty -> a -> a) -> a -> container -> Offset ty -> Offset ty -> a
foldr f !initialAcc ba startIndex endIndex = loop startIndex
  where
    loop !i
        | i == endIndex = initialAcc
        | otherwise     = index ba i `f` loop (i+1)
{-# INLINE foldr #-}

foldl1 :: Indexable container ty => (ty -> ty -> ty) -> container -> Offset ty -> Offset ty -> ty
foldl1 f ba startIndex endIndex = loop (startIndex+1) (index ba startIndex)
  where
    loop !i !acc
        | i == endIndex = acc
        | otherwise     = loop (i+1) (f acc (index ba i))
{-# INLINE foldl1 #-}

filter :: (PrimMonad prim, PrimType ty, Indexable container ty)
       => (ty -> Bool) -> MutableByteArray# (PrimState prim) 
       -> container -> Offset ty -> Offset ty -> prim (CountOf ty)
filter predicate dst src start end = loop azero start
  where
    loop !d !s
        | s == end    = pure (offsetAsSize d)
        | predicate v = primMbaWrite dst d v >> loop (d+Offset 1) (s+Offset 1)
        | otherwise   = loop d (s+Offset 1)
      where
        v = index src s
{-# INLINE filter #-}

all :: Indexable container ty => (ty -> Bool) -> container -> Offset ty -> Offset ty -> Bool
all predicate ba start end = loop start
  where
    loop !i
        | i == end               = True
        | predicate (index ba i) = loop (i+1)
        | otherwise              = False
{-# INLINE all #-}

any :: Indexable container ty => (ty -> Bool) -> container -> Offset ty -> Offset ty -> Bool
any predicate ba start end = loop start
  where
    loop !i
        | i == end               = False
        | predicate (index ba i) = True
        | otherwise              = loop (i+1)
{-# INLINE any #-}
