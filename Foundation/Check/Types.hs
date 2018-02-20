-- |
-- Module      : Foundation.Check.Types
-- License     : BSD-style
-- Maintainer  : Foundation maintainers
--
-- A implementation of a test framework
-- and property expression & testing
--
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Foundation.Check.Types
    ( Test(..)
    , testName
    , fqTestName
    , groupHasSubGroup
    , Check(..)
    , PlanState(..)
    , PropertyResult(..)
    , TestResult(..)
    , HasFailures
    ) where

import           Basement.Imports hiding (throw)
import           Foundation.Collection
import           Foundation.Monad
import           Foundation.Monad.State
import           Foundation.Check.Property
import           Foundation.Check.Gen

-- | Result of a property run
data PropertyResult =
      PropertySuccess
    | PropertyFailed  String
    deriving (Show,Eq)

-- | Name of a test Followed
data TestResult =
      PropertyResult String HasTests       PropertyResult
    | GroupResult    String HasFailures HasTests [TestResult]
    deriving (Show)

-- | number of tests and failures
type HasTests    = CountOf TestResult
type HasFailures = CountOf TestResult

data PlanState = PlanState
    { planRng         :: Word64 -> GenRng
    , planValidations :: CountOf TestResult
    , planParams      :: GenParams
    , planFailures    :: [TestResult]
    }

newtype Check a = Check { runCheck :: StateT PlanState IO a }
    deriving (Functor, Applicative, Monad)
instance MonadState Check where
    type State Check = PlanState
    withState f = Check (withState f)
instance MonadThrow Check where
    throw = Check . throw
instance MonadCatch Check where
    catch (Check ma) handler = Check $ catch ma (runCheck . handler)

-- | different type of tests supported
data Test where
    -- Unit test
    Unit      :: String -> IO () -> Test
    -- Property test
    Property  :: IsProperty prop => String -> prop -> Test
    -- Multiples tests grouped together
    Group     :: String -> [Test] -> Test
    -- Check plan
    CheckPlan :: String -> Check () -> Test

-- | Name of a test
testName :: Test -> String
testName (Unit s _)     = s
testName (Property s _) = s
testName (Group s _)    = s
testName (CheckPlan s _) = s

fqTestName :: [String] -> String
fqTestName = intercalate "/" . reverse

groupHasSubGroup :: [Test] -> Bool
groupHasSubGroup [] = False
groupHasSubGroup (Group{}:_) = True
groupHasSubGroup (_:xs) = groupHasSubGroup xs
